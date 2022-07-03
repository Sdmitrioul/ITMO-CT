"use strict";


const eventPriority = {
    // :NOTE: Меньше чем debug?
    default: -1,
    Debug: 0,
    Info: 1,
    Warning: 2,
    Error: 3
}

let supportedFormats = {
    default: () => "",
    date: function() {
        const currDate = new Date();
        return currDate.toString() + ": ";
    },
    time: function() {
        const currDate = new Date();
        return currDate.getHours() + ":" + currDate.getMinutes() + ":" + currDate.getSeconds() + ": ";
    }
}


function BaseLogger(format = "default", priority = "default") {
    // :NOTE: - setFormat/setPriority
    this.priority = eventPriority[priority];
    this.format = supportedFormats[format];
}

BaseLogger.prototype.setFormat = function(format) {
    if (format in supportedFormats) {
        this.format = supportedFormats[format];
    } else {
        this.format = supportedFormats["default"];
    }
}
BaseLogger.prototype.setPriority = function(event) {
    if (event in eventPriority) {
        this.priority = eventPriority[event];
    }
}
BaseLogger.prototype.log = function(message, event) {
    if (!event || eventPriority[event] >= this.priority) {
        this.logImpl(this.format() + message);
    }
} 


function ConsoleLogger(format, priority) {
    BaseLogger.call(this, format, priority); 
}

ConsoleLogger.prototype = Object.create(BaseLogger.prototype);
ConsoleLogger.prototype.logImpl = function(message) {
    println(message);
}



function HTMLLogger(format, priority) {
    BaseLogger.call(this, format, priority); 
}
HTMLLogger.prototype = Object.create(BaseLogger.prototype);
HTMLLogger.prototype.logImpl = function(message) {
    document.write("<p>" + message + "</p>");
}


function CompositeLogger(...loggers) {
    this.loggers = loggers;
}
CompositeLogger.prototype.log = function(message, event) {
    for (let logger of this.loggers) {
        logger.log(message, event);
    }
} 
CompositeLogger.prototype.setPriority = function(priority) {
    for (let logger of this.loggers) {
        logger.setPriority(priority);
    }
} 
CompositeLogger.prototype.setFormat = function(format) {
    for (let logger of this.loggers) {
        logger.setFormat(format);
    }
}



function demonstrate(logger) {
    logger.log("example message");
    logger.log("another example message");
    logger.setPriority("Error");
    logger.log("this message should be ignored", "Warning");
    logger.log("this message shouldn't be ignored", "Error");
    logger.log("logger does not ignore messages without a level");
    logger.log("trying to set an invalid debug level:");
    logger.setPriority("Low");
}
    
let logger = new ConsoleLogger();
demonstrate(logger);
logger.log("trying to set an invalid format:");
logger.setFormat("format");
logger.log("Same, but with time format:");
logger.setFormat("time");
demonstrate(logger);

demonstrate(new HTMLLogger());

demonstrate(new CompositeLogger(new HTMLLogger("time"), new HTMLLogger(), new ConsoleLogger()));