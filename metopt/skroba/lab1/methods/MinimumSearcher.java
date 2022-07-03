package skroba.lab1.methods;

import skroba.exceptions.TimeOutException;
import skroba.lab1.utils.data.Answer;

public interface MinimumSearcher {
    //PRE:  leftBorder < rightBorder
    Answer findMin(double leftBorder, double rightBorder) throws TimeOutException;
    int getOperationCounter();
    //PRE:  findMin must throw exception
    Answer answerWithException();
}

