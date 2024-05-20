/* Main function and core logic leibowitz-client */

#include <errno.h>
#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netdb.h>
#include <ctype.h>

#include "utils.h"

/* Return the offset to the first non-space character in buf. */
size_t
chomp(char *buf)
{
	size_t i;
	for (i = 0; isspace(buf[i]) && buf[i] != '\0'; i++);
	return i;
}

/* Given buf as a pointer to the first char in an atom, return the
 * offset to the final char of the atom. */
size_t
chomp_atom(char *buf)
{
	size_t i;
	/* FIXME: handle quoted forms! */
	for (i = 0; !isspace(buf[i])
		     && buf[i] != '\0'
		     && buf[i] != '('
		     && buf[i] != ')';
	     i++);
	return i;
}

/* Given buf as a pointer to the beginning " of a string, return the
 * offset to the terminating " */
size_t
chomp_string(char *buf)
{
	size_t i = 1; /* Skip initial " */
	while (buf[i] != '"' && buf[i] != '\0')
		i += (buf[i] == '\\' && buf[i + 1] == '"') ? 2 : 1;
	if (buf[i] == '\0') {
		WARN("parser error: unterminated string\n");
		exit(1);
	}
	return i;
}

void
slynk_parse_message(char *msg)
{
	msg = "(:new-features (:slynk :plump-utf-32 :osicat-fd-streams :cl-who :hunchentoot :sbcl-debug-print-variable-alist :split-sequence :flexi-streams :cl-ppcre :cl-fad :bordeaux-threads :global-vars :chunga cffi-features:flat-namespace cffi-features:x86-64 cffi-features:unix :cffi cffi-sys::flat-namespace alexandria::sequence-emptyp :thread-support :quicklisp :asdf3.3 :asdf3.2 :asdf3.1 :asdf3 :asdf2 :asdf :os-unix :non-base-chars-exist-p :asdf-unicode :arena-allocator :x86-64 :gencgc :64-bit :ansi-cl :common-lisp :elf :ieee-floating-point :linux :little-endian :package-local-nicknames :sb-ldb :sb-package-locks :sb-thread \"An interleaved string!\" :sb-unicode :sbcl :unix \"Another string!\")) \"parse error";
	char *cursor = msg + (uintptr_t)chomp(msg);
	while (*cursor != '\0') {
		/* hurr durr label followed by declaration is a c23
		 * extension */
		size_t offset = 0;
		switch (*cursor) {
		case '(':
			puts("{");
			break;
		case ')':
			puts("}");
			break;
		case ' ':
			cursor += (uintptr_t)chomp(cursor);
			puts("");
			goto continue_no_increment;
		case '"':
			if ((offset = chomp_string(cursor)) == '\0')
				goto continue_no_increment;
			printf("``%.*s''\n", (int)--offset, ++cursor);
			cursor += (uintptr_t)offset;
			break;
		/* case '.': */
		default:
			offset = chomp_atom(cursor);
			for (size_t i = 0; i < offset; i++)
				putchar(cursor[i]);
			cursor += (uintptr_t)offset;
			goto continue_no_increment;
		}
		++cursor;
	continue_no_increment:
		/* drrrrrr label at the end of a compound statement is
		 * a c23 extension */
		continue;
	}
	/* free(msg); */
}

int
slynk_connect(char *host, char *port)
{
	int sock, rc;
	struct addrinfo *servinfo, hints = {
		.ai_family = AF_UNSPEC,	   /* IP v agnostic */
		.ai_socktype = SOCK_STREAM /* TCP */
	};
	if ((rc = getaddrinfo(host, port, &hints, &servinfo)) != 0) {
		fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(rc));
		return -1;
	}
	if (servinfo == NULL) {
		WARN("getaddrinfo didn't return anything\n");
		return -1;
	}
	for (struct addrinfo *i = servinfo; i != NULL; i = i->ai_next) {
		if ((sock = socket(i->ai_family, i->ai_socktype, i->ai_protocol)) == -1) {
			perror("socket");
			continue;
		}
		if (connect(sock, i->ai_addr, i->ai_addrlen) == -1) {
			close(sock);
			perror("connect");
			continue;
		}
		break;
	}
	freeaddrinfo(servinfo);
	return sock;
}

void
slynk_send(int sock, char *msg)
{
	char *raw;
	asprintf(&raw, "%06x%s", (int)strlen(msg), msg);
	INFO("Sending %s\n", raw);
	if (send(sock, raw, strlen(raw), 0) == -1) {
		perror("send");
		exit(1);
	}
	free(raw);
}

char *
slynk_recv(int sock)
{
	size_t bytes , hdr_len = 6, body_len = 0;
	char hdr[hdr_len + 1], *body = NULL;
	if ((bytes = recv(sock, hdr, hdr_len, 0)) == -1) {
		perror("recv");
		exit(1);
	}
	if (bytes != hdr_len)
		WARN("Received invalid header of length %li \"%s\", expected %li bytes\n",
		     bytes, hdr, hdr_len);
	body_len = xstrtol(hdr, NULL, 16, "Header is not a valid hex number");
	body = (char *)xcalloc(body_len + 1, sizeof(char), NULL);
	if ((bytes = recv(sock, body, body_len, 0)) == -1) {
		perror("recv");
		free(body);
		exit(1);
	}
	if (bytes != body_len)
		WARN("Received invalid body of %li bytes, expected %li\n", bytes,
		     body_len);
	return body;
}

void
slynk_disconnect(int sock)
{
	INFO("Disconnecting...\n");
	slynk_send(sock, "(:emacs-rex (cl:format T \"Goodbye, cruel world~%\") nil t 1)");
	slynk_parse_message(slynk_recv(sock));
	slynk_send(sock, "(:emacs-channel-send 1 (:teardown))");
	slynk_parse_message(slynk_recv(sock));
}

/*
 * (let ((msg "(:emacs-rex (cl:format T \"Hello, world!~%\") nil t 1)"))
 *   (format T "~6,'0X~A" (length msg) msg))
 *
 * (:emacs-channel-send 1 (:process "(format T \"hello!~%\")"))
 * (:channel-send 1 (:write-values (("NIL" 0 "'nil"))))
 */

int
main(int argc, char **argv)
{
	int sock;
	char *host = "127.0.0.1", *port = "4005";
	INFO("Connecting to %s:%s\n", host, port);

	if ((sock = slynk_connect(host, port)) < 0) {
		fprintf(stderr, "Failed to connect to %s:%s\n", host, port);
		return 1;
	}
	slynk_send(sock, "(:emacs-rex (cl:format T \"Hello, world!~%\") nil t 1)");
	slynk_parse_message(slynk_recv(sock));
	sleep(5);
	slynk_disconnect(sock);

	close(sock);
	return 0;
}
