#!/bin/sh
ERLDOCGENLIB=./erl_docgen
DATE=`date +"%B %e, %Y"`
#
# generate specs from the Erlang source
escript ${ERLDOCGENLIB}priv/bin/specs_gen.escript ../src/rand.erl
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
    --stringparam appname Rand \
    --stringparam appver 0.1 \
    -path ${ERLDOCGENLIB}/priv/dtd \
    -path ${ERLDOCGENLIB}/priv/dtd_html_entities \
    ${ERLDOCGENLIB}/priv/xsl/db_html.xsl \
    rand.xml
