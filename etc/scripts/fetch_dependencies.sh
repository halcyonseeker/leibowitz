#!/bin/sh

set -e

[ -f "leibowitz.asd" ] || {
	echo "WARNING: script must be run at the root of repository" 2>&1
	exit 1
}

assert_checksum() {
	file="$1"
	known_hash="$2"
	computed_hash="$(sha1sum "$file" | cut -d ' ' -f1 | tr -d '\n')"
	if [ "$known_hash" = "$computed_hash" ]; then
		echo "Checksum okay."
	else
		echo "SHA1 checksum verification failed for $file!"
		echo "Known hash $known_hash ≠ $computed_hash; aborting"
		exit 1
	fi
}

fetch_dependency() {
	name="$(echo "$1" | awk '{print $1}')"
	sha1="$(echo  "$1" | awk '{print $2}')"
	url="$(echo  "$1" | awk '{print $3}' | sed 's/^http/https/')"
	dir="build/dependencies"
	mkdir -p "$dir"
	echo "Fetching $name from $url..."
	curl -O --output-dir "$dir" "$url"
	tarball=$dir/$name-*.tgz
	# assert_checksum "$tarball" "$sha1"
	tar xzf $tarball -C "$dir"
}

while read -r line; do fetch_dependency "$line" ; done < ./etc/dependencies.txt
