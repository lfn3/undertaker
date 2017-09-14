package com.lmax.undertaker.junit.sources;

public interface DoubleSource {
    double getDouble();
    double getDouble(double max);
    double getDouble(double min, double max);

    double getRealDouble();
    double getRealDouble(double max);
    double getRealDouble(double min, double max);
}
