## So

for the following examples

```lisp
(pat-match '(x = (?is ?n numberp)) '(x = 34)) 
(pat-match '(x = (?and (?is ?n numberp) (?is ?n oddp))) '(x = 34))
```

```
(lestrade:wtf 'numberp)
;; =>
;; Symbol: NUMBERP
;; Package: CL
;; NUMBERP :EXTERNAL
```

should be somehow so

```lisp
(fset 'is-number (lambda (x) (numberp x)))
(pat-match '(x = (?is ?n is-numberp)) '(x = 34)) 
(pat-match '(x = (?and (?is ?n is-numberp) (?is ?n oddp))) '(x = 34))
```
