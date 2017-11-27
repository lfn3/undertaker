package net.lfn3.undertaker;

public class OverrunException extends Exception {
    public OverrunException() {
        super();
    }

    public OverrunException(IndexOutOfBoundsException cause) {
        super(cause);
    }
}
