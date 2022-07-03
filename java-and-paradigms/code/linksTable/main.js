"use strict";

const PLUS = "+";
const MINUS = "-";
const MUL = "*";
const DIV = "/";
const OPEN_BRACKET = '(';
const CLOSE_BRACKET = ')';

function EvalError(message) {
    this.message = message;
}
EvalError.prototype = Object.create(Error.prototype);
EvalError.prototype.name = "EvalError";
EvalError.prototype.constructor = EvalError;

function Const(value) {
    this.value = value;
};

Const.prototype.evaluate = function () {
    return this.value;
};

Const.prototype.toString = function () {
    return this.value;
};

function Variable(number) {
    this.number = number;
};

Variable.prototype.evaluate = function (list) {
    if (this.number - 1 >= list.length) {
        throw new EvalError("Index out of bound: " + this.number);
    }
    return list[this.number - 1].evaluate(list);
};

Variable.prototype.toString = function () {
    return "$" + this.number;
};

const Operation = function (...operands) {
    this.getOperands = function () {
        return operands
    }
};


Operation.prototype.evaluate = function (list) {
    return this.doOperation(...this.getOperands().map(operand => operand.evaluate(list)))
};

Operation.prototype.toString = function () {
    return this.operands.map(operand => operand.toString() + " ") + "op";
};

const createOperation = function (doOperation) {
    const result = function (...operands) {
        Operation.apply(this, operands)
    };
    result.prototype = new Operation;
    result.prototype.doOperation = doOperation;
    return result;
};

function ExpressionError(message) {
    this.message = message;
}
ExpressionError.prototype = Object.create(Error.prototype);
ExpressionError.prototype.name = "ExpressionError";
ExpressionError.prototype.constructor = ExpressionError;

const Add = createOperation(function (a, b) {
    return a + b;
});

const Sub = createOperation(function (a, b) {
    return a - b;
});

const Div = createOperation(function (a, b) {
    return a / b;
});

const Mul = createOperation(function (a, b) {
    return a * b;
});

const Neg = createOperation(function (a) {
    return -a;
});

const Parser = function(expression) {
    this.expression = expression;
    this.currentTokenIndex = -1;
    this.expression = this.deleteSpaces();
    return this.thirdLevel();
};

Parser.prototype.deleteSpaces = function() {
    let string = "";
    for(let i = 0; i < this.expression.length; i++) {
        let c = this.expression.charAt(i);
        if (c !== " ") string = string.concat(c);
    }
    return string;
};

Parser.prototype.thirdLevel = function() {
    let third = this.secondLevel();

    while (this.currentTokenIndex < this.expression.length - 1) {
        let operation = this.expression[++this.currentTokenIndex];
        println(operation);
        switch (operation) {
            case (PLUS):
                third = new Add(third, this.secondLevel());
                break;

            case (MINUS):
                third = new Sub(third, this.secondLevel());
                break;

            default:
                this.currentTokenIndex--;
                return third;
        }
    }
    return third;
};

Parser.prototype.secondLevel = function() {
    let second = this.firstLevel();
    while (this.currentTokenIndex < this.expression.length - 1) {
        let operation = this.expression[++this.currentTokenIndex];
        println(operation);
        switch (operation) {
            case (MUL) :
                second = new Mul(second, this.firstLevel());
                break;

            case (DIV) :
                second = new Div(second, this.firstLevel());
                break;

            default :
                this.currentTokenIndex--;
                return second;
        }
    }
    return second;
};

Parser.prototype.firstLevel = function() {
    let first = this.expression[++this.currentTokenIndex];
    switch (first) {
        case ("$") :
            let i = 1;
            while (i + this.currentTokenIndex < this.expression.length && Number.isInteger(Number.parseInt(this.expression[i + this.currentTokenIndex]))) {
                i++
            }
            if (i === 1) {
                throw new ExpressionError("Isn't expected: " + this.expression.substr(this.currentTokenIndex));
            } else {
                i -= 1;
                this.currentTokenIndex += i;
                return new Variable(Number.parseInt(this.expression.substr(this.currentTokenIndex - i + 1, i)));
            }
        case (MINUS) :
            if (this.currentTokenIndex + 1 < this.expression.length) {
                return new Neg(this.firstLevel());
            } else {
                throw new ExpressionError("Isn't expected m: " + this.expression.substr(this.currentTokenIndex));
            }

        case ("1") :
        case ("2") :
        case ("3") :
        case ("4") :
        case ("5") :
        case ("6") :
        case ("7") :
        case ("8") :
        case ("9") :
            let j = 1;
            while (j + this.currentTokenIndex < this.expression.length && Number.isInteger(Number.parseInt(this.expression[j + this.currentTokenIndex]))) {
                j++
            }
            this.currentTokenIndex += j - 1;
            return new Const(Number.parseInt(this.expression.substr(this.currentTokenIndex - j + 1, j)));

        case (OPEN_BRACKET) :
            let tmp = this.thirdLevel();
            this.currentTokenIndex++;
            return tmp;

        case (CLOSE_BRACKET) :
            throw new ExpressionError("Isn't expected: " + this.expression.substr(this.currentTokenIndex));
        default :
            throw new ExpressionError("Isn't expected: " + this.expression.substr(this.currentTokenIndex));
    }
};

const makeCL = function (string) {
    let mas = string.split("|");
    return mas;
};

function table(string) {
    let mass = makeCL(string);
    //println(mass);
    for (let i = 0; i < mass.length; i++) {
        mass[i] = new Parser(mass[i]);
    }
    let ans = "";
    for (let j = 0; j < mass.length; j++) {
        if (j !== mass.length -1) {
            ans = ans + mass[j].evaluate(mass) + "|";
        } else {
            ans = ans + mass[j].evaluate(mass);
        }
        //println(ans);
    }
    return ans;
}