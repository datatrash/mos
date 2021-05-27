.test ok {
    .assert 1 == 1 "nice"
 	brk
}

.test fail {
    .assert 1 == 2 "whoops"
}