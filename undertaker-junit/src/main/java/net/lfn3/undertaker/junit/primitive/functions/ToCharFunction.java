package net.lfn3.undertaker.junit.primitive.functions;

@FunctionalInterface
public interface ToCharFunction<T> {
    char applyAsChar(T value);
}
