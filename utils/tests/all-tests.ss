(import
  :clan/utils/tests/base-test
  :clan/utils/tests/date-test
  :clan/utils/tests/generator-test
  :clan/utils/tests/list-test
  :clan/utils/tests/logger-test
  :clan/utils/tests/number-test
  :clan/utils/tests/stateful-avl-map-test
  :clan/utils/tests/temporary-files-test)

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
