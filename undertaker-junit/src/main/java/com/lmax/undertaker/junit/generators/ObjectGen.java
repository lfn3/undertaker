package com.lmax.undertaker.junit.generators;

@FunctionalInterface
public interface ObjectGen {
    <T> T generate(Generator<T> generator);
}
