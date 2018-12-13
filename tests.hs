import Test.HUnit
import Hw4

flattenTest_ExampleTest1 = TestCase(assertEqual "flattenTest_ExampleTest1" [5] ( flatten(Elem 5) ))
flattenTest_ExampleTest2 = TestCase(assertEqual "flattenTest_ExampleTest2" [1,2,3,4,5] ( flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) ))

flattenTest_EmptyList = TestCase(assertEqual "flattenTest_EmptyList" ([]::[Int]) ( flatten(List []) ))

flattenTest_FlatList = TestCase(assertEqual "flattenTest_FlatList" [1,2,3] ( flatten(List [Elem 1, Elem 2, Elem 3]) ))
flattenTest_CharList = TestCase(assertEqual "flattenTest_CharList" ['a','s','d'] ( flatten(List [Elem 'a', Elem 's', Elem 'd']) ))
flattenTest_StringList = TestCase(assertEqual "flattenTest_CharList" ["qwe","asd","zxc"] ( flatten(List [Elem "qwe", Elem "asd", Elem "zxc"]) ))
flattenTest_DoubleList = TestCase(assertEqual "flattenTest_CharList" [1.0,1.005,2.5] ( flatten(List [Elem 1.0, Elem 1.005, Elem 2.5]) ))
flattenTest_NegativeList = TestCase(assertEqual "flattenTest_NegativeList" [(-1),(-2),(-3)] ( flatten(List [Elem (-1), Elem (-2), Elem (-3)]) ))
flattenTest_PairsList = TestCase(assertEqual "flattenTest_PairsList" [(0,1),(2,3),(4,5)] ( flatten(List [Elem (0, 1), List[Elem (2, 3), Elem (4, 5)]]) ))

flattenTest_EmptyChildList = TestCase(assertEqual "flattenTest_EmptyChildList" [1,2,3] ( flatten(List [Elem 1, List[], Elem 2, Elem 3]) ))
flattenTest_SomeNestedLists = TestCase(assertEqual "flattenTest_SomeNestedLists" [0] ( flatten(List [List [List [List [List [Elem 0]]]]]) ))

flattenTest_CopyElements = TestCase(assertEqual "flattenTest_CopyElements" [1,1,1] ( flatten(List [Elem 1, List [Elem 1], Elem 1]) ))


tests = TestList [
    TestLabel "flattenTest_ExampleTest1" flattenTest_ExampleTest1,
    TestLabel "flattenTest_ExampleTest2" flattenTest_ExampleTest2,
    TestLabel "flattenTest_EmptyList" flattenTest_EmptyList,
    TestLabel "flattenTest_FlatList" flattenTest_FlatList,
    TestLabel "flattenTest_CharList" flattenTest_CharList,
    TestLabel "flattenTest_StringList" flattenTest_StringList,
    TestLabel "flattenTest_DoubleList" flattenTest_DoubleList,
    TestLabel "flattenTest_NegativeList" flattenTest_NegativeList,
    TestLabel "flattenTest_PairsList" flattenTest_PairsList,
    TestLabel "flattenTest_EmptyChildList" flattenTest_EmptyChildList,
    TestLabel "flattenTest_SomeNestedLists" flattenTest_SomeNestedLists,
    TestLabel "flattenTest_CopyElements" flattenTest_CopyElements
  ]