(import
  :clan/utils/tests/base-test
  :clan/utils/tests/date-test
  :clan/utils/tests/exception-test
  :clan/utils/tests/generator-test
  :clan/utils/tests/list-test
  :clan/utils/tests/logger-test
  :clan/utils/tests/memo-test
  :clan/utils/tests/number-test
  :clan/utils/tests/peekable-iterator-test
  :clan/utils/tests/stateful-avl-map-test
  :clan/utils/tests/temporary-files-test)

(export
  unit-tests)

(def unit-tests
  [base-test
   date-test
   exception-test
   generator-test
   list-test
   logger-test
   memo-test
   number-test
   peekable-iterator-test
   stateful-avl-map-test
   temporary-files-test
   ])
