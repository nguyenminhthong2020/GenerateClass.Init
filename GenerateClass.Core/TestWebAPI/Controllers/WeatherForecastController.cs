//using .MyApp.GeneratedDtos;
using GenerateClass.Generator.Attributes;
using Microsoft.AspNetCore.Mvc;
using MyApp.GeneratedDtos;
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
            //return Enumerable.Range(1, 5).Select(index => new WeatherForecast
            //{
            //    Date = DateTime.Now.AddDays(index),
            //    TemperatureC = Random.Shared.Next(-20, 55),
            //    Summary = Summaries[Random.Shared.Next(Summaries.Length)]
            //})
            //.ToArray();
            return Ok();
        }

        //[NonAction]
        //[GenerateClass(GenerateClass.Generator.Enums.Mode.All)]
        //public async Task<IActionResult> CreateStudent1(Student student, School school)
        //{
        //    var obj = new Teacher() { Phone = "0123456789", Mail = "a@gmail.com" };
        //    Family obj1 = new();
        //    obj1.Adress = "100/1A, Le Duan street...";
        //    List<Area> list1 = new List<Area>() { };
        //    obj1.Students = new List<Student>() { };
        //    obj1.Classes = new List<TeacherClass>() { };
        //    var result = new ResponseResult() { NewID = "abcde", isCheck = true };
        //    return Ok(result);
        //}

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
        }

        /// <summary>
        /// Chưa test các kiểu: enum,struct, record
        /// </summary>
        /// <param name="student"></param>
        /// <param name="school"></param>
        /// <returns></returns>
        [NonAction]
        [GenerateClass(GenerateClass.Generator.Enums.Mode.All)]
        public IEnumerable<(WeatherForecast, IEnumerable<ResponseResult>)> CreateStudent2(Student student, School school)
        {
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
            decimal[][] myArr = [[1.5m, 2m, 3M],  [0.6M, 123M, 400m], [18m, 6m]];
            result.Arr = myArr;
            decimal[,] myArr1 = new decimal[,]
            {
                { 1.5m, 2m, 3m },
                { 0.6m, 123m, 400m },
                { 18m, 6m, 0m }
            };
            result.Arr1 = myArr1;
            result.Arr2 = new byte[] {1,2,3,};
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

            var randomList = "abced".Select((c, i) => new { Index = i, Value = c}).ToList();
            //var randomObject = new MyRandomObject(12, "abc", DateTime.Now, false);
            var randomObject1 = new MyRandomObject1(Age: 12, Name: "abc", Time: DateTime.Now, Checked: false);
            var randomObject2 = new MyRandomObject2(Id: 12, Name: "abc", Time: DateTime.Now, Checked: false);

            WeatherForecast defaultWeater = default;
            var response = Enumerable.Range(0, 3).Select(x => (defaultWeater, Enumerable.Repeat(result, 2)));
            return response;
        }
    }
}
