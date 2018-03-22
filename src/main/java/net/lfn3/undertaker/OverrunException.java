package net.lfn3.undertaker;

public class OverrunException extends RuntimeException {
    public OverrunException() {
        super();
    }

    public OverrunException(IndexOutOfBoundsException cause) {
        super(cause);
    }
}
