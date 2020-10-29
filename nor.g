new grammar Norwegian
goal ms

ms -> s . => 1
ms -> np , np vp/np. => ( (4 3) 1 )  # topicalized
s -> np vp => (2 1)
s/np -> vp => (lambda x (1 x))
s/np -> np vp/np => (2 1)

compp -> comp s => 2
compp/np -> comp s/np => 2

np -> name => 1
np -> det n-bar => ! 1 (lambda x (1 &var (2 &var) x))
n-bar -> n => (lambda x (1 x))
n-bar -> n-bar compp/np => (lambda x (& (1 x) (2 x)))

vp -> v1 => (lambda x (1 x))
vp -> v2 np => (lambda x (1 x 2))
vp -> v3 np pp => (lambda x (1 x 2 3))
vp -> v3 np => (lambda x (1 x 2 ?))  #missing indirect object
vp -> v3 np np => (lambda x (1 x 3 2))   # dative

vp/np -> v2 => (lambda y (lambda x (1 y x)))
vp/np -> v3 pp => (lambda y (lambda x (1 y x 2)))
vp/np -> v3 => (lambda y (lambda x (1 y x ?)))
vp/np -> v3 np => (lambda y (lambda x (1 y 2 x)))

pp -> p np => 2


# passives
byp -> by np => 2
vp -> aux-be vp-pass => 2
vp-pass -> v3 => (lambda x (1 ? x ?))
vp-pass -> v3 byp => (lambda x (1 2 x ?))
vp-pass -> v3 pp => (lambda x (1 ? x 2))
vp-pass -> v3 pp byp => (lambda x (1 3 x 2))
vp-pass -> v3 byp pp => (lambda x (1 2 x 3))
vp-pass -> v3 np => (lambda x (1 ? 2 x))

