;;; -*- mode: Lisp; coding: utf-8  -*-

(tao:tao)


(aaa
 bbb
 ccc
 #_d
 #_g)
#_g (ggg hhh iii)
#_d (ddd eee fff)

==> (AAA BBB CCC (DDD EEE FFF) (GGG HHH III))

(aaa
 bbb
 #_
 ccc)
(ddd eee #_)
(ggg hhh iii)

==> (AAA BBB (DDD EEE (GGG HHH III)) CCC)

(aaa bbb #_kkk ccc)
#_kkk (ddd eee fff)

==> (AAA BBB (DDD EEE FFF) CCC)

(defclass foo ()
  (#_a #_b #_c))

(a :initform 0)
(b :initform 1)
(c :initform 2)

==> (DEFCLASS FOO () ((A :INITFORM 0) (B :INITFORM 1) (C :INITFORM 2)))

'(aa bb #.kk cc)
#.kk
foo
bar
baz
end-of-kk

==> (AA BB FOO BAR BAZ CC)

(!aSpaceShip
   (obj-let ((x 0) (y 0))
#.mmh
#.methods

\@ ))

#.mmh
mmh0
mmh1
mmh2
end-of-mmh

#.methods
method0
method1
method2
end-of-methods



==>
(!ASPACESHIP
 (OBJ-LET ((X 0) (Y 0))
   METHOD0
   METHOD1
   METHOD2
   MMH0
   MMH1
   MMH2
   @))

(aa bb #.kk cc)
#.kk
(dd)
ee
(fff ggg)
end-of-kk
==> (AA BB (DD) EE (FFF GGG) CC)


