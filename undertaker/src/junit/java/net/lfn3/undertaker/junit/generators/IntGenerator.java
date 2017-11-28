package net.lfn3.undertaker.junit.generators;

import net.lfn3.undertaker.junit.Source;

import java.util.function.ToIntFunction;

@FunctionalInterface
public interface IntGenerator extends ToIntFunction<Source> {
}
