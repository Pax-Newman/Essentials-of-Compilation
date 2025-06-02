import ast
from typing import Any, assert_never

import pytest
import utils

# ---- Language

## ---- AST Node Checking

def is_leaf(node: ast.expr) -> bool:
    match node:
        case ast.Constant(n):
            return True
        case ast.Call(ast.Name("input_int"), []):
            return True
        case ast.UnaryOp(ast.USub(), e1):
            return False
        case ast.BinOp(e1, ast.Add(), e2):
            return False
        case ast.BinOp(e1, ast.Sub(), e2):
            return False
        case _:
            assert_never(node)  # pyright: ignore[reportArgumentType]


def is_expression(node: ast.AST) -> bool:
    match node:
        case ast.Constant(n):
            return True
        case ast.Call(ast.Name("input_int"), []):
            return True
        case ast.UnaryOp(ast.USub(), e1):
            return is_expression(e1)
        case ast.BinOp(e1, ast.Add() | ast.Sub(), e2):
            return is_expression(e1) and is_expression(e2)
        case _:
            return False


def is_statement(node: ast.AST) -> bool:
    match node:
        case ast.Expr(ast.Call(ast.Name("print"), [e])):
            return is_expression(e)
        case ast.Expr(e):
            return is_expression(e)
        case _:
            return False


def is_Lint(program: ast.AST) -> bool:
    match program:
        case ast.Module(body):
            return all(is_statement(node) for node in body)
        case _:
            return False

## ---- Interpreter

def interpret_expression(expression: ast.expr) -> Any:
    match expression:
        case ast.Constant(n):
            return n
        case ast.UnaryOp(ast.USub(), right):
            return utils.neg64(interpret_expression(right))
        case ast.BinOp(left, ast.Add(), right):
            left_val, right_val = (
                interpret_expression(left),
                interpret_expression(right),
            )
            return utils.add64(left_val, right_val)
        case ast.BinOp(left, ast.Sub(), right):
            left_val, right_val = (
                interpret_expression(left),
                interpret_expression(right),
            )
            return utils.sub64(left_val, right_val)
        case ast.Call(ast.Name("input_int"), []):
            return utils.input_int()


def interpret_statement(statement: ast.stmt):
    match statement:
        case ast.Expr(ast.Call(ast.Name("print"), [arg])):
            print(interpret_expression(arg))
        case ast.Expr(value):
            interpret_expression(value)


def interpret_Lint(program: ast.mod):
    match program:
        case ast.Module(body):
            for stmt in body:
                interpret_statement(stmt)

## ---- Partial Evaluation

def pe_neg(right_child: ast.expr):
    match right_child:
        case ast.Constant(n):
            return ast.Constant(utils.neg64(n))
        case _:
            return ast.UnaryOp(ast.USub(), right_child)


def pe_add(left_child: ast.expr, right_child: ast.expr):
    match (left_child, right_child):
        case (ast.Constant(l), ast.Constant(r)):
            return ast.Constant(utils.add64(l, r))
        case _:
            return ast.BinOp(left_child, ast.Add(), right_child)


def pe_sub(left_child: ast.expr, right_child: ast.expr):
    match (left_child, right_child):
        case (ast.Constant(l), ast.Constant(r)):
            return ast.Constant(utils.sub64(l, r))
        case _:
            return ast.BinOp(left_child, ast.Sub(), right_child)


def pe_exp(expression: ast.expr):
    match expression:
        case ast.UnaryOp(ast.USub(), right):
            return pe_neg(pe_exp(right))

        case ast.BinOp(left, ast.Add(), right):
            return pe_add(pe_exp(left), pe_exp(right))

        case ast.BinOp(left, ast.Sub(), right):
            return pe_sub(pe_exp(left), pe_exp(right))

        case ast.Constant(n):
            return expression

        case ast.Call(ast.Name("input_int"), []):
            return expression

        case _:
            return expression

def pe_stmt(statement: ast.stmt):
    match statement:
        case ast.Expr(ast.Call(ast.Name("print"), [arg])):
            return ast.Expr(ast.Call(ast.Name("print"), [pe_exp(arg)]))
        case ast.Expr(value):
            return ast.Expr(pe_exp(value))
        case _:
            assert_never(statement)

def pe_Pint(program: ast.mod):
    match program:
        case ast.Module(body):
            partially_evaluated_body = [pe_stmt(statement) for statement in body]
            return ast.Module(partially_evaluated_body)
        case _:
            assert_never(program)


interpret_Lint(ast.parse("print(10 + -(12 + (2 + 20)))"))
interpret_Lint(pe_Pint(ast.parse("print(10 + -(12 + (2 + 20)))")))

interpret_Lint(ast.parse("print(10 + -(12 + (20 - 2)))"))
interpret_Lint(pe_Pint(ast.parse("print(10 + -(12 + (20 - 2)))")))


# ---- Tests


## ---- is_expression

@pytest.mark.parametrize("expression", [
    ast.Constant(1),
    ast.Call(ast.Name("input_int"), []),
    ast.UnaryOp(ast.USub(), ast.Constant(1)),
    ast.BinOp(ast.Constant(1), ast.Add(), ast.Constant(2)),
    ast.BinOp(ast.Constant(1), ast.Sub(), ast.Constant(2)),
])
def test_is_expression(expression):
    assert is_expression(expression)

@pytest.mark.parametrize("expression", [
    ast.BinOp(ast.Constant(1), ast.Mult(), ast.Constant(2)),
    ast.BinOp(ast.Constant(1), ast.Div(), ast.Constant(2)),
    ast.Call(ast.Name("print"), [ast.Constant(1)]),
])
def test_is_expression_invalid(expression):
    assert not is_expression(expression)

## ---- is_statement

@pytest.mark.parametrize("statement", [
    ast.Expr(ast.Call(ast.Name("print"), [ast.Constant(1)])),
    ast.Expr(ast.Constant(1)),
])
def test_is_statement_expr(statement):
    assert is_statement(statement)

@pytest.mark.parametrize("statement", [
    ast.BinOp(ast.Constant(1), ast.Add(), ast.Constant(2)),
    ast.Call(ast.Name("print"), [ast.Constant(1)]),
])
def test_is_statement_invalid(statement):
    assert not is_statement(statement)

## ---- is_Lint
def test_is_L_int_valid():
    program = ast.Module(
        body=[
            ast.Expr(ast.Call(ast.Name("print"), [ast.Constant(1)])),
            ast.Expr(ast.UnaryOp(ast.USub(), ast.Constant(2))),
            ast.Expr(ast.BinOp(ast.Constant(3), ast.Add(), ast.Constant(4))),
        ]
    )
    assert is_Lint(program)


def test_is_Lint_invalid():
    program = ast.Module(
        body=[
            ast.Expr(ast.BinOp(ast.Constant(3), ast.Mult(), ast.Constant(1))),
        ]
    )
    assert not is_Lint(program)

## ---- pe_Pint

@pytest.mark.parametrize("program",[
    "10 + -(12 + 20)",
    "10 + -(12 + (20 - 2))",
])
def test_pe_Pint(program):
    program_ast = ast.parse(program)

    interpreted_result = interpret_Lint(program_ast)
    pe_result = interpret_Lint(pe_Pint(program_ast))

    assert interpreted_result == pe_result

