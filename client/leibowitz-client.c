/* Main function and core logic leibowitz-client */

#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netdb.h>

#include "utils.h"
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

void
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
	body_len = strtol(hdr, NULL, 16);
	if (errno)
		WARN("Received invalid header \"%s\" (strtol: %s)\n",
		     hdr, strerror(errno));
	if ((body = (char *)calloc(body_len + 1, sizeof(char))) == NULL) {
		perror("calloc");
		exit(1);
	}
	if ((bytes = recv(sock, body, body_len, 0)) == -1) {
		perror("recv");
		free(body);
		exit(1);
	}
	if (bytes != body_len)
		WARN("Received invalid body of %li bytes, expected %li\n", bytes,
		     body_len);

	printf("%.80s", body);
	bytes > 80 ? printf("[elided %li]\n", bytes - 80) : puts("");
	free(body);
}

void
slynk_disconnect(int sock)
{
	INFO("Disconnecting...\n");
	slynk_send(sock, "(:emacs-rex (cl:format T \"Goodbye, cruel world~%\") nil t 1)");
	slynk_recv(sock);
	slynk_send(sock, "(:emacs-channel-send 1 (:teardown))");
	slynk_recv(sock);
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
	slynk_recv(sock);
	sleep(5);
	slynk_disconnect(sock);

	close(sock);
	return 0;
}
