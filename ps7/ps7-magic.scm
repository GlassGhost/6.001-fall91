;; PS7-MAGIC.SCM --- Magic stuff to atone for the for the sins of the compiler.
;;                   That is, compiled TAIL and pre-loaded STREAM hacks lose.

(enable-language-features)

(load "psets:chiptail")			; Make TAIL type check (be interpreted)
(load "bin:stream")			; Make stream hacks use this new TAIL

(disable-language-features)
