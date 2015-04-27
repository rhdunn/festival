#!/bin/sh

if [ x${AUTOMAKE_DIR} = x ] ; then
	AUTOMAKE_DIR=`find /usr/share/automake* -maxdepth 0 -type d 2>&1 | sort | tail -n 1`
fi

if [ ! -e "${AUTOMAKE_DIR}" -o ! -e "${AUTOMAKE_DIR}/install-sh" ] ; then
	echo "Unable to locate the automake directory."
	echo "Set AUTOMAKE_DIR to the correct automake directory path."
	exit 1
fi

ln -sf CHANGELOG.md NEWS

ln -sf ${AUTOMAKE_DIR}/config.guess .
ln -sf ${AUTOMAKE_DIR}/config.sub .

ln -sf ${AUTOMAKE_DIR}/install-sh .
ln -sf ${AUTOMAKE_DIR}/missing .
ln -sf ${AUTOMAKE_DIR}/mkinstalldirs .

autoconf
