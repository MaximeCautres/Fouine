#!/bin/bash
tar --transform='s|src|rendu4_CAUTRES_GOUTAGNY|' -cf rendu4_CAUTRES_GOUTAGNY.tar src &&
gzip --best rendu4_CAUTRES_GOUTAGNY.tar

