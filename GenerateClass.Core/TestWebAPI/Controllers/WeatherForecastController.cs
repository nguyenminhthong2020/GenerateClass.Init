using GenerateClass.Generator.Attributes;
using GenerateClass.Generator.Enums;
using Microsoft.AspNetCore.Mvc;
using MyApp.ModelsDtos;
using MyApp.ModelsDtos1;
using System.Collections.Concurrent;
using TestWebAPI.Enum;

namespace TestWebAPI.Controllers
{
    [ApiController]
    [Route("[controller]")]
    public class WeatherForecastController : ControllerBase
    {
        private static readonly string[] Summaries = new[]
        {
            "Freezing", "Bracing", "Chilly", "Cool", "Mild", "Warm", "Balmy", "Hot", "Sweltering", "Scorching"
        };

        private readonly ILogger<WeatherForecastController> _logger;

        public WeatherForecastController(ILogger<WeatherForecastController> logger)
        {
            _logger = logger;
        }

        [HttpGet]
        [Route("GetWeatherForecast")]
        public IActionResult Get()
        {
            _ = Enumerable.Range(1, 5).Select(index => new WeatherForecast
            {
                Date = DateTime.Now.AddDays(index),
                TemperatureC = Random.Shared.Next(-20, 55),
                Summary = Summaries[Random.Shared.Next(Summaries.Length)]
            })
            .ToArray();
            return Ok();
        }

        [NonAction]
        [GenerateClass("ConfigIdGoku", GenerateClass.Generator.Enums.Mode.All)]
        public async Task<IActionResult> CreateStudent1(Student student, School school, MyTestPara<(int, MyTestPara1), MyTestPara2, MyTestPara3<string>> mytestPara)
        {
            student.HomeMail = "a@gmail.com";
            var school1 = new School() { SchoolMail = "abc@gmail.com" };
            var school2 = new School(Id: 10, SchoolPhone: "0123456789");
            var school3 = new School(SchoolMail: "xyz@gmail.com", SchoolPhone: "0123456789");
            var school4 = new School(SchoolMail4: "", SchoolPhone4: true, SchoolTest4: "hahah");

            var testEnumList = new MyTestEnumList() { Prop1 = TestEnum.Y, Prop2 = "a", Prop0 = (-13, "abc", "xyz", false), Prop00 = (Prop00X: 1, Prop00Y: 'c') };
            var dict = new Dictionary<string, (string, List<(int, char)>)>
            {
                { "mykey", ("a", new List<(int, char)> { (1, 'a') }) }
            };
            var testEnumLista = new MyTestEnumList(Prop1: TestEnum.X, PropZ: (TestEnum.Y, -123.567, 1, dict));
            var testEnumListb = new MyTestEnumList();
            var testEnumListc = new MyTestEnumList() { };
            var testEnumListd = new MyTestEnumList()
            {
                PropZZ = (TestEnum.Y, -123.567, null, dict),
                PropZZZ = new
                {
                    A = 1,
                    B = 'X'
                },
                PropZZZZ = "abced".Select((c, i) => new { Index = i, Value = c }).ToList()
            };
            MyTestEnumList testEnumListe = new();
            testEnumListe.PropFinalFlash = (1, "abc", null);
            testEnumListe.PropKamehameha = (1, "abc", 2025, false);
            testEnumListe.PropMakankosapo = (1, "abc", null, (1, 'a'));
            testEnumListe.Galickgun = (1, "abc", null, (1, 'a', null));

            var firstTest = new FirstTest()
            {
                TestObj10 = Array.Empty<int>(),
                TestObj11 = "abcedf"
            };

            var secondTest = new SecondText()
            {
                Prop1 = new MyRandomObject1(RandomObject1Property: 'x')
            };

            var result = new ResponseResult() { };
            return Ok(result);



            var myobjX = new MyObjX();
            var myobjM = new MyObjM(Age: 30, Birthday: "");
            var myobjZ = new MyObjZ(AgeZ: 30, BirthdayZ: "");

            var testRecord = new TestRecord<int, int>()
            {
                Items = [1, 2],
                Item2 = 1
            };
            var myTestRecord = new MyTestRecord(Prop: testRecord);
            return default;
        }


        [NonAction]
        [GenerateClass("ConfigIdGoku", GenerateClass.Generator.Enums.Mode.All)]
        public (
        IEnumerable<(IEnumerable<ResponseClass>, Dictionary<string, (string, List<ResponseClass1>)>)>,
        ResponseClass2,
        ResponseClass3<int>,
        ResponseClass4<int, char>
        )
        CreateStudent2
        (
            Student student,
            School school,
            List<int> mylist,
            List<Test1> myList1,
            Dictionary<string, (string, List<(int, string)>)> myDict,
            Dictionary<string, (string, List<(int, Test2)>)> myDict1,
            (Test3, int) testTuple,
            TestClass testlass,
            TestGenericClass<int> testClass1,
            TestGenericClass<(int, string)> testClass2
        )
        {
            var bodyObject = new TestGenericObject1<int>() { };
            var bodyObject1 = new TestGenericObject1<string>() { };
            var bodyObject2 = new TestGenericObject2<(int, string)>() { };
            var bodyObject3 = new TestGenericObject2<(int, Dictionary<int, (string, List<int>)>)>() { };
            var bodyObject4 = new TestGenericObject2<(int, Dictionary<string, (string, List<MyNewClass>)>)>() { };

            var firstTest = new FirstTest()
            {
                Area = new Area() { },
                City = new City() { },
                TestObj1 = null,
                TestObj2 = (int?)1,
                TestObj3 = (int?)null,
                TestObj4 = (object?)null,
                TestObj5 = (string?)"",
                TestObj6 = (DateTime?)DateTime.Now,
                TestObj7 = MyEnum.Value3,
                TestObj8 = new MyStruct(3.6, 4.8),
                TestObj9 = TestEnum.Y
            };

            var obj = new Teacher() { Phone = "0123456789", Mail = "a@gmail.com", Age = 36, Dt = DateTime.Now };
            Family obj1 = new();
            obj1.Adress = "100/1A, Le Duan street...";
            List<Area> list1 = new List<Area>() {
                 new Area() {
                     AreProperty1 = -16.789f,
                     AreaProperty2 = DateOnly.MinValue,
                     AreaProperty3 = new List<List<string>>()
                    {
                        new List<string>() { "a", "b", "c" },
                        new List<string>() { "d", "e", "f" },
                        new List<string>() { "g", "h", "i" }
                    },
                     AreaProperty4 = "xyz".Select((c, i) => new { Index = i, Value = c}).ToList(),
                     AreaProperty5 = (object)"1",
                     AreaProperty6 = new
                     {
                         A = 1,
                         B = "X"
                     }
                 }
            };
            obj1.Students = new List<Student>() { };
            obj1.Classes = new List<TeacherClass>() { };
            var result = new ResponseResult() { NewID = "abcde", isCheck = true };
            result.RandomNumber1 = 21.9;
            result.RandomNumber2 = -12.9;
            result.RandomNumber3 = double.MaxValue;
            result.RandomNumber4 = (long)16.5;
            result.RandomNumber5 = -16.789f;
            result.Dict = "abcde".ToDictionary(c => c, c => (int)c);
            result.Hashset = "abcde".ToHashSet();
            decimal[][] myArr = [[1.5m, 2m, 3M], [0.6M, 123M, 400m], [18m, 6m]];
            result.Arr = myArr;
            decimal[,] myArr1 = new decimal[,]
            {
                { 1.5m, 2m, 3m },
                { 0.6m, 123m, 400m },
                { 18m, 6m, 0m }
            };
            result.Arr1 = myArr1;
            result.Arr2 = new byte[] { 1, 2, 3, };
            result.Arr3 = new short[] { 1, 3, 4 };
            result.myList1 = new List<List<string>>()
            {
                new List<string>() { "a", "b", "c" },
                new List<string>() { "d", "e", "f" },
                new List<string>() { "g", "h", "i" }
            };
            result.myList2 = new List<List<(string, int)>>()
            {
                new List<(string, int)>() { ("a", 12), ("b", 33), ("c", -123) },
                new List<(string, int)>() { ("d", 68), ("e", 0), ("f", 66) },
                new List<(string, int)>() { ("g", 49), ("h", 78), ("i", -123) }
            };
            result.myList3 = new List<(int, char)>()
            {
                (12, 'a'), (235, 'c')
            };
            result.obj = (object)567;

            result.Dt = DateTime.Now.AddDays(-1).AddMinutes(5);

            var randomList = "abced".Select((c, i) => new { Index = i, Value = c }).ToList();
            //var randomObject = new MyRandomObject(12, "abc", DateTime.Now, false);
            var randomObject1 = new MyRandomObject1(Age: 12, Name: "abc", Time: DateTime.Now, Checked: false);
            var randomObject2 = new MyRandomObject2(Id: 12, Name: "abc", Time: DateTime.Now, Checked: false);

            WeatherForecast defaultWeater = default;
            var response = Enumerable.Range(0, 3).Select(x => (defaultWeater, Enumerable.Repeat(result, 2)));
            //return response;

            var school4Copy = new School(SchoolMail4: "t", SchoolPhone4: true, SchoolTest4: "uu");
            var school5 = new School("xyz@gmail.com", false, "0123456789");
            var school6 = new School(SchoolMail6: "xyz@gmail.com", SchoolPhone6: "0123456789", S6: true, S6x: 123);
            student.NewStudentProperty = "abcxyed";
            school.NewSchoolProperty = "abcxyed";
            var myobjY = new MyObjY();
            var myobjN = new MyObjN(Age1: 30, Birthday1: "");
            MyObjZ myObjZ = new();
            MyObjZ myObjZ1 = new MyObjZ();
            MyObjZ myObjZ2 = new MyObjZ() { };
            myObjZ.FinalBoss = "Goku";

            var testEnumList1 = new MyTestEnumList() { Prop3 = new List<int>() { 3, 4, 5 } };
            var initialData = new Dictionary<int, int> { { 6, 8 } };
            var testEnumList2 = new MyTestEnumList(Prop4: true, Prop5: new ConcurrentDictionary<int, int>(initialData));
            var testEnumList3 = new MyTestEnumList(Prop4: false, Prop6: new List<char>() { 'a', 'b' }, Prop7: 3_000_000_000, Prop8: 30_000_000_000);

            MyTestPara3<char> myTestPara3 = new();


            var testForTestPara3 = new ConcurrentBag<int>() { 1 };
            myTestPara3.NewPropertyTest3 = (false, DateTime.Now, null, new List<int>() { 1 }, (-12f, testForTestPara3, null));
            return default;
        }


        [NonAction]
        [GenerateClass("ConfigIdGoku", GenerateClass.Generator.Enums.Mode.All)]
        public async Task<StudentABC> CreateStudent3()
        {
            var testRecord1 = new TestRecord<char, int>()
            {
                Items = ['a', 'b', 'c'],
                Item2 = 1
            };
            var myTestRecord = new MyTestRecord() { Prop1 = testRecord1 };
            return default;
        }


        [NonAction]
        [GenerateClass("ConfigIdGoku", GenerateClass.Generator.Enums.Mode.All)]
        public StudentXYZ CreateStudent4()
        {
            var TestlocalFunction1 = (int x, LocalFunction1 lcf1) =>
            {
                var tlf1x = new LocalFunction1x
                {
                    Prop1 = 1,
                    Prop2 = "a",
                    Prop3 = (1, "abc", null),
                    Prop4 = (1, false),
                    PropNewNew = 12_921,
                    PropNewNew1 = "b",
                };
                var tlf1y = new LocalFunction1y(
                                 Prop1: 1,
                                 Prop2: new LocalFunction1yClass()
                                 {
                                     Prop1Y = "a",
                                     Prop1YY = (1, "a", null, new List<int>() { 1, 2, 3 })
                                 }
                                 );
                return new LocalFunction1z(
                                 Prop1: 123,
                                 Prop2: new LocalFunction1zClass()
                                 {
                                     Prop1Z = "a",
                                     Prop1ZZ = new Prop1ZZClass() { PropFinal = false, PropFinal1 = (2, "x", null) },
                                     Prop1ZZZ = (1, "abc", null),
                                 });
            };


            string TestlocalFunction2(int x, LocalFunction2 lcf1)
            {
                var tlf1yNew = new LocalFunction1y(Prop3: 100);

                var tlf1x = new LocalFunction2x
                {
                    Prop1 = 1,
                    Prop2 = "a",
                    Prop3 = (1, "abc", null),
                    Prop4 = (1, false)
                };
                var tlf1y = new LocalFunction2y(
                                 Prop1: 1,
                                 Prop2: new LocalFunction2yClass()
                                 {
                                     Prop1Y = "a",
                                     Prop1YY = (1, "a", null, new List<int>() { 1, 2, 3 })
                                 }
                                 );
                _ = new LocalFunction2z(
                                 Prop1: 123,
                                 Prop2: new LocalFunction2zClass()
                                 {
                                     Prop1Z = "a",
                                     Prop1ZZ = new Prop2ZZClass() { PropFinal = false, PropFinal1 = (2, "x", null) },
                                     Prop1ZZZ = (1, "abc", null),
                                 });

                return "";
            }
        ;
            LocalFunction3 TestlocalFunction3(int x, LocalFunction3x lcf3)
            {
                return default;
            }
        ;

            var testLambdaExpression1 = new List<(int Key, ValueLambda Value)>() {
                                                 (1, new ValueLambda(){Prop1 = 2, Prop2 = "x"}),
                                                 (2, new ValueLambda(){Prop3 = 5, Prop4 = (1, "Abc", null, ('a', false, null, new int[]{1,2,3 }))}),
                                          }
                                         .Select(x => new TestLambdaExpression1() { Prop1 = x.Key })
                                         .ToList();
            var testLambdaExpression2 = new List<int>() { 1, 2, 3 }
                                 .Select(x => new TestLambdaExpression2(Prop2: x)).ToList();
            var testLambdaStatement1 = new List<int>() { 5, 6 }.Select(x =>
            {
                var obj1 = new TestLambdaStatement1() { Prop1 = x };
                var obj2 = new TestLambdaStatement1x(Prop2: "a");
                return new TestLambdaStatement1y(Obj1: obj1, Obj2: obj2);
            }).ToList();
            string s = "abc";
            new List<int>() { 1, 2, 3 }.ForEach(a =>
            {
                object tt = default;
                var newTestObjList = new newTestObjListClass(A1: "a", B1: (1, "a", tt, ('k', null)));
                s += a;
            });

            Action<(TestAction, string), int> TestDelegateAction1 = (para1, para2)
                    => Console.WriteLine($"Para1: {para1.Item2}, Para2: {para2}");

            Action<(TestAction, string), int> TestDelegateAction2 = (para1, para2) =>
            {
                var obj1 = new DelegateAction2() { Prop1 = para1.Item1 };
                var obj1x = new DelegateAction2(Prop1x: para1.Item1);
                var obj2 = new DelegateAction2() { Prop2 = (1, "abc", null), prop3 = (1, "a", (true, 3, null)) };
                var obj3 = new DelegateAction2x()
                {
                    Prop1 = new List<(int, InsideClass<int, string>)>() { (1, new()), (2, new()) }
                };
                Console.WriteLine($"Para1: {para1.Item2}, Para2: {para2}"); ;
            };

            //Func<(int, (string, TestFunc1)), int, string> testDelegateFunc1 = (a, b) => string.Empty;
            Func<(TestFunc2, string), int, int> TestDelegateFunc2a = (para1, para2) =>
            {
                var obj1New = new DelegateAction2() { Prop1New = para1.Item1 };

                var obj2 = new DelegateFunc2x() { Prop2 = (1, "abc", null), prop3 = (1, "a", (true, 3, "null")) };
                var obj3 = new DelegateFunc2y()
                {
                    Prop1 = new List<int>() { 1, 2, 3 }
                };

                var TestlocalFunction1Za = (int x, LocalFunction1z lcf1) =>
                {
                    var tlf1yZ = new LocalFunction1y(
                                     1,
                                     new LocalFunction1yClass()
                                     {
                                         Prop1Y = "a",
                                         Prop1YY = (1, "a", null, new List<int>() { 1, 2, 3 })
                                     }
                                     );
                    var tlf1yZZZ = new LocalFunction1yZZ(
                                     PropA: 1,
                                     PropB: new LocalFunction1yClass(Prop1YY1: (1, "a", null, new List<int>() { 1, 2, 3 }))
                                     );
                    _ = new LocalFunction1z(
                                     Prop55: "K",
                                     Prop66: new LocalFunction1zClass()
                                     {
                                         Prop1Z = "a",
                                         Prop1ZZ = new Prop1ZZClass() { PropFinal = false, PropFinal1 = (2, "x", null) },
                                         Prop1ZZZ = (1, "abc", null),
                                     });
                };

                return 1000;
            };

            void TestComplexFunc<T>(
            IEnumerable<T> data,
            Func<T, bool> filter,
            Action<T> action,
            Func<TestComplexFunc1, int> func1,
            Action<TestComplexFunc2> action1,
            Func<(TestComplexFunc2, int), int> func2)
            {
                foreach (var item in data.Where(filter))
                {
                    var TestlocalFunction1ZGoku = (int x, LocalFunction1ZGoku lcf1) =>
                    {
                        var tlf1yZZZ = new LocalFunction1yZZ(
                                         1,
                                         new LocalFunction1yClass(Prop1YY1: (1, "a", null, new List<int>() { 1, 2, 3 }))
                                         );
                        return new LocalFunction1ZGoku(
                                         Prop55: "K",
                                         Prop66: new LocalFunction1zClass()
                                         {
                                             Prop1Z = "a",
                                             Prop1ZZ = new Prop1ZZClass() { PropFinal = false, PropFinal1 = (2, "x", null) },
                                             Prop1ZZZ = (1, "abc", null),
                                         });
                    };
                    action(item);
                }

                string s1 = "abc";
                new List<int>() { 1, 2, 3 }.ForEach(a =>
                {
                    object tt1 = default;
                    var newTestObjList1 = new newTestObjListClass1(A1: "a", B1: (1, "a", tt1, ('k', null)));
                    s1 += a;
                });
            }

            return default;
        }

        [NonAction]
        [GenerateClass("ConfigIdVegeta", GenerateClass.Generator.Enums.Mode.All)]
        public async Task<IActionResult> CreateStudent5()
        {
            var obj1 = new TestCreateStudent5a();
            var obj2 = new TestCreateStudent5a() { };
            var obj3 = new TestCreateStudent5a() {  Prop1 = 1, Prop2 = "a"};

            var obj4 = new TestCreateStudent5a(Prop3: 1, Prop4 : "a");
            var obj5 = new TestCreateStudent5a(2, "b");

            var obj6 = new TestCreateStudent5a("b", 1);
            var obj7 = new TestCreateStudent5a() { Prop1 = 10, Prop4 = "aa" };

            var object1 = new TestCreateStudent5b(1, "a");

            // special case: null, object
            //var obj8 = new TestCreateStudent5a()
            return default;
        }

        /// <summary>
        /// JWT, Redis, Kafka, HttpClient, Dapper, EF, MongoDB, Quartz
        /// Newtonsoft.Json, DynamicExpresso, 
        /// NPOI / DotNetCore.NPOI, CsvHelper, EPPlus, HtmlToOpenXml, DiffMatchPatch, DocumentFormat.OpenXml
        /// QRCoder, System.Drawing.Common, ZXing.Net
        /// </summary>
        /// <returns></returns>
        [NonAction]
        [GenerateClass("ConfigIdVegeta", Mode.Body | Mode.Parameter)]
        public async Task<IActionResult> TestOther(TestOther1 testother1)
        {

            return default;
        }

        public enum MyEnum
        {
            Value1,
            Value2,
            Value3
        }

        public struct MyStruct
        {
            public MyStruct(double x, double y)
            {
                X = x;
                Y = y;
            }
            public double X { get; }
            public double Y { get; }
            public override string ToString() => $"({X}, {Y})";
        };
    }
}
