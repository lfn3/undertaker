package net.lfn3.undertaker.junit.sources;

import net.lfn3.undertaker.junit.Generator;
import net.lfn3.undertaker.junit.Source;

import java.util.Map;
import java.util.function.BiFunction;

public interface MapSource {
    <K, V> Map<K, V> getMap(Generator<K> keyGenerator, Generator<V> valueGenerator);
    <K, V> Map<K, V> getMap(Generator<K> keyGenerator, BiFunction<Source, K, V> valueGenerator);
}
