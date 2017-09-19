package com.lmax.undertaker.junit;

import com.lmax.undertaker.junit.sources.*;
import org.junit.rules.TestRule;

import java.util.Map;

public interface Source extends BoolSource,
                                BoolArraySource,
                                ByteSource,
                                ByteArraySource,
                                ShortSource,
                                ShortArraySource,
                                IntSource,
                                IntArraySource,
                                CharSource,
                                CharArraySource,
                                LongSource,
                                LongArraySource,
                                FloatSource,
                                FloatArraySource,
                                DoubleSource,
                                DoubleArraySource,
                                StringSource,
                                ListSource,
                                ArraySource,
                                ObjectSource,
                                MapSource,
                                EnumSource,
                                FromCollectionSource,
                                TestRule {
    long pushInterval(String intervalName);

    void popInterval(long intervalId, Object generatedValue);

    static Source create() {
        return new SourceRule();
    }

    static Source create(Map<Class, Generator> defaultGenerators) {
        return new SourceRule(defaultGenerators);
    }
}
