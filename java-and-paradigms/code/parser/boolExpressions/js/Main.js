"use strict";

const OPERATIONS = {
    Negate : "~",
    Conj : "&",
    Disj : "|"
};

const OPEN_BRACKET = '(';
const CLOSE_BRACKET = ')';

let Variables = [];

const Const = function (value) {
    this.getValue = function () {
        return value;
    }
};

Const.prototype.priority = function () {
    return 1;
};

Const.prototype.evaluate = function () {
    return this.getValue() === 1;
};

Const.prototype.push = function () {
    if (this.getValue() === 1) return new Const(0);
    else return new Const(1);
};

Const.prototype.toString = function () {
    return this.getValue().toString()
};

const Variable = function (name) {
    this.getName = function () {
        return name;
    }
};

Variable.prototype.priority = function () {
    return 1;
};

Variable.prototype.push = function () {
    return new Negate(new Variable(this.getName()));
};

Variable.prototype.evaluate = function (...variables) {
    return variables[Variables.indexOf(this.getName())]
};

Variable.prototype.toString = function () {
    return this.getName()
};

const Operation = function (...operands) {
    this.getOperands = function () {
        return operands
    }
};

Operation.prototype.evaluate = function (...variables) {
    return this.doOperation(...this.getOperands().map(operand => operand.evaluate(...variables)))
};

Operation.prototype.push = function() {
    return this.doPush(...this.getOperands())
};

Operation.prototype.priority = function() {
    return this._priority();
};

Operation.prototype.toString = function () {
    return this.tostring(...this.getOperands())
};

const createOperation = function (doOperation, doPush, tostring, pri) {
    const result = function (...operands) {
        Operation.apply(this, operands)
    };
    result.prototype = new Operation;
    result.prototype.doOperation = doOperation;
    result.prototype.doPush = doPush;
    result.prototype.tostring = tostring;
    result.prototype._priority = pri;
    return result;
};

const Conj = createOperation(
    (a, b) => a && b,
    function (a, b) { return new Disj(a.push(), b.push())},
    function (a, b) { if (a.priority() > 2) a = "(" + a.toString() + ")";
                                if (b.priority() > 2) b = "(" + b.toString() + ")";
                                    return a + " & " + b},
    () => 2
);

const Disj = createOperation(
    (a, b) => a || b,
    function (a, b) { return new Conj(a.push(), b.push())},
    (a, b) => a.toString() + " | " + b.toString(),
    () => 3
);

const Negate = createOperation(
    (a) => !a,
    function (a) { return a },
    function (a) { if (a.priority() > 1) return "~(" + a.toString() + ")";
                            else return "~" + a.toString()},
    () => 1
);

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
        switch (operation) {
            case (OPERATIONS.Disj):
                third = new Disj(third, this.secondLevel());
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
        switch (operation) {
            case (OPERATIONS.Conj) :
                second = new Conj(second, this.firstLevel());
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
            while (i + this.currentTokenIndex < this.expression.length && this.expression[i + this.currentTokenIndex])
            return new Const(1);
        case ("0") :
            return new Const(0);
        case (OPERATIONS.Negate) :
            return this.firstLevel().push();
        case (OPEN_BRACKET) :
            let tmp = this.thirdLevel();
            this.currentTokenIndex++;
            return tmp;
        default :
            if (Variables.indexOf(first) === -1) Variables.push(first);
            return new Variable(first);
    }
};

const MakeKNForDNF = function(expression) {
    this.expression = expression;
};

MakeKNForDNF.prototype.answearKNF = function() {
    let arr = this._tableTrue();
    let r = Variables.length;
    let n = Math.pow(2, r);
    let result = "";
    for (let i = 0; i < n; i++) {
        if (!arr[i][r]) {
            result = result.concat("(");
            for (let j = 0; j < r; j++) {
                if (!arr[i][j]) result = result.concat(Variables[j] + "|");
                else result = result.concat("~" + Variables[j] + "|");
            }
            result = result.slice(0, -1);
            result = result.concat(")&");
        }
    }
    result = result.slice(0, -1);
    return result.toString();
};

MakeKNForDNF.prototype.answearDNF = function() {
    let arr = this._tableTrue();
    let r = Variables.length;
    let n = Math.pow(2, r);
    let result = "";
    for (let i = 0; i < n; i++) {
        if (arr[i][r]) {
            result = result.concat("(");
            for (let j = 0; j < r; j++) {
                if (arr[i][j]) result = result.concat(Variables[j] + "&");
                else result = result.concat("~" + Variables[j] + "&");
            }
            result = result.slice(0, -1);
            result = result.concat(")|");
        }
    }
    result = result.slice(0, -1);
    return result.toString();
};

MakeKNForDNF.prototype._tableTrue = function() {
    let n = Variables.length;
    let str = generate("", n);
    let arr = [];
    arr[0] = [];
    let j = 0;
    while (j < str.length / n) {
        for (let i = 0; i < n; i++) {
            if (str.charAt(j * n + i) === "0") arr[j].push(false);
            else arr[j].push(true);
        }
        arr[j].push(this.expression.evaluate(...arr[j]));
        j++;
        arr.push([]);
    }
    return arr;
};



const Inc = function(expression) {

};

const generate = function(string, length) {
    if (length === 0) return string;
    else return generate(string.concat(0), length - 1).concat(generate(string.concat(1), length - 1));
};

let a = "~(1 & ~a) | (b | c) & d";

let h = new Parser(a);

let b = new Negate(new Conj(new Const(1), new Negate(new Variable("x"))));