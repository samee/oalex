The files in this directory consist of two different kinds of tests:

  * Files on which `oalex` is expected to produce error.
  * Files on which `oalex` succeeds. They can test for both success or
    failure of user languages using appropriate examples.

The first kind, on which `oalex` is expected to fail, has comments of the form
"Expected syntax error:". That directive is used by oalex_main_test.py.
