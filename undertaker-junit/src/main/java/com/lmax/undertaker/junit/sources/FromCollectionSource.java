package com.lmax.undertaker.junit.sources;

import java.util.Collection;

public interface FromCollectionSource {
    <T> T from(Collection<T> collection);
    <T> T from(T[] collection);
}
