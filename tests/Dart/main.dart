import './src/ArithmeticTests.fs.dart' as arithmetic;
import './src/ArrayTests.fs.dart' as array;
import './src/ComparisonTests.fs.dart' as comparison;
import './src/DateTimeTests.fs.dart' as date;
import './src/DictionaryTests.fs.dart' as dictionary;
import './src/HashSetTests.fs.dart' as hash_set;
import './src/ListTests.fs.dart' as list;
import './src/MapTests.fs.dart' as map;
import './src/RegexTests.fs.dart' as regex;
import './src/SeqTests.fs.dart' as seq;
import './src/SetTests.fs.dart' as set$;
import './src/SudokuTests.fs.dart' as sudoku;
import './src/TailCallTests.fs.dart' as tailcall;
import './src/TupleTests.fs.dart' as tuple;
import './src/UnionTests.fs.dart' as union;

void main() {
  arithmetic.tests();
  array.tests();
  comparison.tests();
  date.tests();
  dictionary.tests();
  hash_set.tests();
  list.tests();
  map.tests();
  regex.tests();
  seq.tests();
  set$.tests();
  sudoku.tests();
  tailcall.tests();
  tuple.tests();
  union.tests();
}