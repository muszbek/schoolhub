# Integrated testing

The tests are meant to be run with common test.

```shell
ct_run -spec local.spec

erl -name ct@localhost -setcookie test_cookie -eval "ct_master:run(\"dist.spec\")" -run init stop
```
