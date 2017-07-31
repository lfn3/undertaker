package undertaker.junit.generators;

import undertaker.junit.Source;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

public interface ListGen
{
    public <T> List<T> getList(Function<Source, T> generator);
}
