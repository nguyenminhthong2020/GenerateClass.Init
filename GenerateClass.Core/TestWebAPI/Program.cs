WebApplicationBuilder builder = WebApplication.CreateBuilder(args);

// Add services to the container.

builder.Services.AddControllers();
// Learn more about configuring Swagger/OpenAPI at https://aka.ms/aspnetcore/swashbuckle
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();

builder.Services.AddGenerateClass(config =>
   new[] {
    new GenerateClassOption() {
        ConfigId = "ConfigIdGoku",
        Enabled = true,
        Path = "./Models/Dtos/",
        Namespace = "MyApp.ModelsDtos",
        RequiredProperties = new() {
            { "Id", "long" },
            { "Name1", typeof(string) },
            { "Name2", typeof(String) },
            { "CollerationId1", "int" },
            { "CollerationId2", typeof(Int16) },
            { "CollerationId3", "Int64" },
            { "CustomList1", typeof(List<int>) },
            { "CustomList2", "List<int>" },
            { "CustomDictionary", typeof(Dictionary<string, char>) },
        }
    },
    new GenerateClassOption() {
        ConfigId = "ConfigIdVegeta",
        Enabled = true,
        Path = "./Models/Dtos1/",
        Namespace = "MyApp.ModelsDtos1",
        RequiredProperties = new() {
            { "Age", "short" },
            { "City", "string" },
            { "Country", typeof(String)
            },
        }
    }
   }
);

WebApplication app = builder.Build();

// Configure the HTTP request pipeline.
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseAuthorization();

app.MapControllers();

app.Run();
