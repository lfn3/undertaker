package com.lmax.undertaker.junit;

import com.lmax.undertaker.junit.generators.ArrayGen;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.function.Function;
import java.util.function.Supplier;

public class ArrayGenImpl implements ArrayGen {
    private final Source s;

    public ArrayGenImpl(Source s) {
        this.s = s;
    }

    @Override
    public <T> T[] getArray(Class<T> klass, Function<Source, T> generator) {
        ArrayList<T> result = new ArrayList<>();
        s.repeatedly(() -> result.add(generator.apply(s)));
        return result.toArray((T[]) Array.newInstance(klass, 0));
    }
}
