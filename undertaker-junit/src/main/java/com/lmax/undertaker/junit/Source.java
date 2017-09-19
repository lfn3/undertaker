package com.lmax.undertaker.junit;

import com.lmax.undertaker.junit.sources.*;
import org.junit.rules.TestRule;

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
                                TestRule {
    long pushInterval(String intervalName);

    void popInterval(long intervalId, Object generatedValue);
}
