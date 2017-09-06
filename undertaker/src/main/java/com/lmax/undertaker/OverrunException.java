package com.lmax.undertaker;

public class OverrunException extends Exception {
    public OverrunException() {
        super();
    }

    public OverrunException(IndexOutOfBoundsException cause) {
        super(cause);
    }
}
