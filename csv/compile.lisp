(mapc #'require
  '(asdf asdf-install alexandria babel bordeaux-threads cl-fad cl-ppcre
  local-time sb-aclrepl sb-bsd-sockets sb-cltl2 sb-concurrency
  sb-cover sb-executable sb-grovel sb-introspect sb-md5 sb-posix sb-queue
  sb-rotate-byte sb-rt sb-simple-streams sb-sprof trivial-gray-streams
  flexi-streams zip rucksack))

(require :util)
(require :kensin)

(save-lisp-and-die "sbcl.core-for-slime")
