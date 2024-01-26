import Foundation

indirect enum LogicOperation {
    case value(Bool)
    case variable(Character)
    case not(LogicOperation)
    case and(LogicOperation, LogicOperation)
    case or(LogicOperation, LogicOperation)
    case xor(LogicOperation, LogicOperation)
    case implies(LogicOperation, LogicOperation)
    case equals(LogicOperation, LogicOperation)
    case notEquals(LogicOperation, LogicOperation)
}

var symbolDict: [String: Character] = [
    "and": "∧",
    "not": "¬",
    "or": "∨",
    "xor": "⊕",
    "imply": "→",
    "implies": "→",
    "equals": "=",
    "notequals": "≠",
    "true": "⊤",
    "false": "⊥",
]

typealias BinaryOperation = (LogicOperation, LogicOperation) -> LogicOperation

let binaryOperationDict: [Character: BinaryOperation] = [
    "∧": { (lhs, rhs) in return .and(lhs, rhs) },
    "∨": { (lhs, rhs) in return .or(lhs, rhs) },
    "⊕": { (lhs, rhs) in return .xor(lhs, rhs) },
    "→": { (lhs, rhs) in return .implies(lhs, rhs) },
    "=": { (lhs, rhs) in return .equals(lhs, rhs) },
    "≠": { (lhs, rhs) in return .notEquals(lhs, rhs) }
]

let priorityDict: [Character: Int] = [
    "T": 0b00001,
    "F": 0b00001,
    "∧": 0b00010,
    "∨": 0b00100,
    "⊕": 0b00100,
    "→": 0b00100,
    "=": 0b01000,
    "≠": 0b01000
]

let binaryOpOrder = binaryOperationDict
    .compactMap { (character, operation) in (character, operation, priorityDict[character]) }
    .filter { (_, _, priority) in priority != nil }
    .map { (character, operation, priority) in (character, operation, priority!) }
    .sorted { lhs, rhs in
        let (_, _, lhsPriority) = lhs
        let (_, _, rhsPriority) = rhs
        return lhsPriority > rhsPriority
    }

enum GroupingError: Error {
    case noOpening
    case noClosing
}

func checkBalance(_ str: String) throws {
    var stack: [Character] = []

    for char in str {
        if char == "(" {
            stack.append(char)
        }

        if char == ")" {
            guard stack.popLast() != nil else {
                throw GroupingError.noOpening
            }
        }
    }

    guard stack.isEmpty else {
        throw GroupingError.noClosing
    }
}

func parseUngrouped(formula str: String) -> [Range<Int>] {
    var ranges: [Range<Int>] = []

    var grouping = 0
    var currentRange: Range<Int>? = nil

    for index in (0 ..< str.count) {
        guard let char = str.getCharAt(index) else {
            fatalError("No character at \(index)")
        }

        switch char {
        case ")":
            grouping -= 1
        case "(":
            if let range = currentRange, grouping == 0 {
                ranges.append(range.lowerBound ..< index)
                currentRange = nil
            }

            grouping += 1
        default:
            if grouping == 0, currentRange == nil {
                currentRange = (index ..< str.count)
            }
        }
    }

    if let currentRange = currentRange {
        ranges.append(currentRange)
    }

    return ranges
}

func parse(_ formula: String) throws -> LogicOperation {

    func createNode(_ str: inout String) throws -> LogicOperation {
        try checkBalance(str)

        let trimmedStr = str.trim()

        if trimmedStr.canBeUngrouped() {
            let strCopy = str
            str = trimmedStr.slice(from: 1, to: trimmedStr.count - 1)

            do {
                try checkBalance(str)
                return try createNode(&str)
            } catch {
                str = strCopy
            }
        }

        let ungroupedRanges = parseUngrouped(formula: str)

        for (opChar, operation, _) in binaryOpOrder {
            for range in ungroupedRanges {
                for index in range {
                    guard let char = str.getCharAt(index) else {
                        fatalError("No character at \(index)")
                    }

                    if char != opChar {
                        continue
                    }

                    var leftSlice = str.slice(from: 0, to: index - 1)
                    var rightSlice = str.slice(from: index + 1, to: str.count)

                    guard let leftNode = try? createNode(&leftSlice) else {
                        fatalError("\(opChar) is missing argument #1")
                    }

                    guard let rightNode = try? createNode(&rightSlice) else {
                        fatalError("\(opChar) is missing argument #2")
                    }

                    return operation(leftNode, rightNode)
                }
            }
        }

        for range in ungroupedRanges {
            for index in range {
                guard let char = str.getCharAt(index) else {
                    fatalError("No character at \(index)")
                }

                if char != "¬" {
                    continue
                }

                var rightSlice = str.slice(from: index + 1, to: str.count)

                guard let rightNode = try? createNode(&rightSlice) else {
                    fatalError("¬ is missing argument #1 `\(rightSlice)`")
                }

                return .not(rightNode)
            }
        }

        if trimmedStr.count == 1, let char = trimmedStr.first {
            switch char {
            case "⊤":
                return .value(true)
            case "⊥":
                return .value(false)
            default:
                return .variable(char)
            }
        }

        fatalError("Failed to create node with `\(str)`")
    }

    var copy = formula
    return try createNode(&copy)
}

func findVars(operations: LogicOperation) -> [Character] {
    var vars: [Character] = []
   
    func explore(operations: LogicOperation) {
        switch operations {
        case .value:
            break
        case .variable(let variable):
            vars.append(variable)
        case .not(let rhs):
            explore(operations: rhs)
        case .and(let lhs, let rhs),
             .or(let lhs, let rhs),
             .xor(let lhs, let rhs),
             .implies(let lhs, let rhs),
             .equals(let lhs, let rhs),
             .notEquals(let lhs, let rhs):
            explore(operations: lhs)
            explore(operations: rhs)
        }
    }
    explore(operations: operations)
    return vars
}

func compute(operations: LogicOperation, variables: [Character: Bool]) -> Bool {
    switch operations {
    case .value(let value):
        return value
    case .variable(let variable):
        guard let value = variables[variable] else {
            fatalError("Variable \(variable) does not have a value.")
        }
        return value
    case .not(let operand):
        return !compute(operations: operand, variables: variables)
    case .and(let lhs, let rhs):
        return compute(operations: lhs, variables: variables) && compute(operations: rhs, variables: variables)
    case .or(let lhs, let rhs):
        return compute(operations: lhs, variables: variables) || compute(operations: rhs, variables: variables)
    case .xor(let lhs, let rhs):
        return compute(operations: lhs, variables: variables) != compute(operations: rhs, variables: variables)
    case .implies(let lhs, let rhs):
        return !compute(operations: lhs, variables: variables) || compute(operations: rhs, variables: variables)
    case .equals(let lhs, let rhs):
        return compute(operations: lhs, variables: variables) == compute(operations: rhs, variables: variables)
    case .notEquals(let lhs, let rhs):
        return compute(operations: lhs, variables: variables) != compute(operations: rhs, variables: variables)
    }
}

while var input = readLine() {
    for (key, value) in symbolDict {
        input = input.replacingOccurrences(of: key, with: "\(value)")
    }

    let parsedFormula = try parse(input)
    let formulaVariables = findVars(operations: parsedFormula)

    let outputRowCount = Int(pow(2, Double(formulaVariables.count)))
    var rows: [RowData] = []

    for rowValue in (0..<outputRowCount) {
        var variables: [Character: Bool] = [:]

        for variableIndex in (0..<formulaVariables.count) {
            let variable = formulaVariables[variableIndex]
            let value = (rowValue & (1 << variableIndex)) != 0
            variables[variable] = value
        }

        let finalValue = compute(operations: parsedFormula, variables: variables)
        rows.append((variables, finalValue))
    }

    printTable(formula: input, variables: formulaVariables, rows: rows)
}

typealias RowData = ([Character: Bool], Bool)

func printTable(formula: String, variables: [Character], rows: [RowData]) {
    print("｜ ", terminator: "")
   
    for variable in variables {
        print("\(variable) ｜ ", terminator: "")
    }
   
    print("\(formula) ｜")

    let separatorLength = (variables.count * 5) + (formula.count) + 5
    print(String(repeating: "-", count: separatorLength))

    for (vars, value) in rows {
        print("｜", terminator: "")
       
        for variable in variables {
            guard let value = vars[variable] else {
                fatalError("Missing variable \(variable) in output")
            }
           
            print(" \(value ? "T" : "F") ｜", terminator: "")
        }

        let result = value ? "T" : "F"
        let padding = (formula.count - 1) / 2
        print(" \(String(repeating: " ", count: padding))\(result)\(String(repeating: " ", count: padding)) ｜")
    }

    print(String(repeating: "-", count: separatorLength))
}

extension Character {
    func isGrouping() -> Bool {
        return self == "(" || self == ")"
    }

    func getGrouping() -> Character? {
        return isGrouping() ? self : nil
    }
}

extension String {
    func canBeUngrouped() -> Bool {
        if let firstGrouping = self.first?.getGrouping(), let lastGrouping = self.last?.getGrouping() {
            return firstGrouping != lastGrouping
        }

        return false
    }

    func trim() -> String {
        var start: Int? = nil
        var end: Int? = nil

        for x in (0 ..< self.count) {
            if let char = self.getCharAt(x), !char.isWhitespace {
                if start == nil {
                    start = x
                }

                end = x + 1
            }
        }

        return slice(from: start ?? 0, to: end ?? self.count)
    }

    func getCharAt(_ charIndex: Int) -> Character? {
        if charIndex > self.count {
            return nil
        }

        return self[self.index(self.startIndex, offsetBy: charIndex)]
    }

    func slice(from: Int, to: Int) -> String {
        precondition(from < to, "Slice cannot create empty strings.")
        precondition(to <= self.count, "Out of bounds for String slice.")

        let startIndex = self.index(self.startIndex, offsetBy: from)
        let endIndex = self.index(self.startIndex, offsetBy: to)

        return String(self[startIndex ..< endIndex])
    }
}
