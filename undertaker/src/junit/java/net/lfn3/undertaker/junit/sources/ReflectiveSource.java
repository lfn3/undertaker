package net.lfn3.undertaker.junit.sources;

import java.lang.reflect.Constructor;

public interface ReflectiveSource {
    <T> T reflectively(Class<T> klass);
    <T> T reflectively(Constructor<T> constructor);
}
