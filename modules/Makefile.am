## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
##
## This file is part of Guile-QR-Code.
##
## Guile-QR-Code is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## Guile-QR-Code is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Guile-QR-Code.  If not, see <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/am/guile.am

SUBDIRS = \
	qr-code \
	.

SOURCES = \
	qr-code.scm

moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

### Makefile.am ends here.
