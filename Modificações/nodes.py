class Node:
    pass

class ProgramNode(Node):
    def __init__(self, declarations):
        self.declarations = declarations

class VarDeclNode(Node):
    def __init__(self, name, var_type, value_node):
        self.name = name
        self.var_type = var_type
        self.value_node = value_node

class LiteralNode(Node):
    def __init__(self, value, type):
        self.value = value
        self.type = type

class BinOpNode(Node):
    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right

class PrintNode(Node):
    def __init__(self, value_node):
        self.value_node = value_node

class IfNode(Node):
    def __init__(self, condition, then_block, else_block=None):
        self.condition = condition
        self.then_block = then_block
        self.else_block = else_block

class WhileNode(Node):
    def __init__(self, condition, block):
        self.condition = condition
        self.block = block

class AssignNode(Node):
    def __init__(self, name, value):
        self.name = name
        self.value = value

class ReturnNode(Node):
    def __init__(self, value):
        self.value = value

class FunctionNode(Node):
    def __init__(self, name, params, return_type, block):
        self.name = name
        self.params = params
        self.return_type = return_type
        self.block = block

class CallNode(Node):
    def __init__(self, callee, arguments):
        self.callee = callee    
        self.arguments = arguments