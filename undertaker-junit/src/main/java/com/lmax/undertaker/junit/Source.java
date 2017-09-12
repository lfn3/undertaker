package com.lmax.undertaker.junit;

import org.junit.rules.TestRule;
import com.lmax.undertaker.junit.generators.*;

public interface Source extends BoolGen,
                                BoolArrayGen,
                                ByteGen,
                                ByteArrayGen,
                                ShortGen,
                                ShortArrayGen,
                                IntGen,
                                IntArrayGen,
                                CharGen,
                                CharArrayGen,
                                LongGen,
                                LongArrayGen,
                                FloatGen,
                                FloatArrayGen,
                                DoubleGen,
                                DoubleArrayGen,
                                StringGen,
                                ListGen,
                                TestRule {
    long pushInterval(String intervalName);

    void popInterval(long intervalId, Object generatedValue);
}
