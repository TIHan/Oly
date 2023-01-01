public interface IExample
{
    void GenericExample<T>(T x);
}

public interface IExample2
{
    void GenericExample<T>(T x) where T : IExample;
}

public interface IExample3
{
    void GenericExample<T>(T x) where T : IExample, IExample2;
}
