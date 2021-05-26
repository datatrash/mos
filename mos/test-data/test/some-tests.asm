.test ok {
    .assert 1 == 1 "nice"
 	rts
}

.test fail {
    .assert 1 == 2 "whoops"
}