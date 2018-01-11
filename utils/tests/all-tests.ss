(import
  :utils/tests/base-test
  :utils/tests/date-test
  :utils/tests/generator-test
  :utils/tests/list-test
  :utils/tests/logger-test
  :utils/tests/number-test
  :utils/tests/stateful-avl-map-test
  :utils/tests/temporary-files-test)

(export
  unit-tests)

(def unit-tests
  [base-test
   date-test
   generator-test
   list-test
   logger-test
   number-test
   stateful-avl-map-test
   temporary-files-test
   ])
