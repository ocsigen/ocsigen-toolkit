opam pin add --no-action ocsigen-toolkit .
opam install --deps-only ocsigen-toolkit
opam install --verbose ocsigen-toolkit

do_build_doc () {
  make doc
  cp -Rf doc/manual-wiki/*.wiki ${MANUAL_SRC_DIR}
  mkdir -p ${API_DIR}/server ${API_DIR}/client
  cp -Rf doc/server/wiki/*.wiki ${API_DIR}/server/
  cp -Rf doc/client/wiki/*.wiki ${API_DIR}/client/
#  cp -Rf doc/index.wiki ${API_DIR}/

}

do_remove () {
  opam remove --verbose ocsigen-toolkit
}
