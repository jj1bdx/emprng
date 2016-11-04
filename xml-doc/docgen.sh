#!/bin/sh
ERLDOCGENLIB=`escript ./erl_docgen_lib.escript`
DATE=`date +"%B %e, %Y"`
#
# generate specs from the Erlang source
escript ./specs_gen.escript ../src/rand.erl
#
xsltproc --noout --stringparam outdir . \
    --stringparam docgen ${ERLDOCGENLIB} \
    --stringparam topdocdir . \
    --stringparam pdfdir . \
    --xinclude --stringparam specs_file "`pwd`/specs.xml" \
    --stringparam stylesheet otp_doc.css \
    --stringparam winprefix Erlang \
    --stringparam logo erlang-logo.png \
    --stringparam pdfname pdfname \
    --stringparam gendate "${DATE}" \
    --stringparam appname emprng \
    --stringparam appver 0.6.0 \
    -path ${ERLDOCGENLIB}/priv/dtd \
    -path ${ERLDOCGENLIB}/priv/dtd_html_entities \
    ${ERLDOCGENLIB}/priv/xsl/db_html.xsl \
    rand.xml
