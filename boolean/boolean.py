import ply.yacc as yacc
import ply.lex as lex

def p_expression_conjunction(p):
    'expression : expression AND expression'
    p[0] = Expression(p[1].clauses + p[3].clauses)

def p_expression_paren(p):
    'expression : "(" expression ")"'
    p[0] = p[2]

def p_expression_clause(p):
    'expression : clause'
    p[0] = Expression([p[1]])

def p_clause_implication(p):
    'clause : unit IMPLIES unit'
    negation = not p[1][1]
    p[0] = Clause(p[1][0], negation, p[3][0], p[3][1])

def p_clause_disjunction(p):
    'clause : unit OR unit'
    p[0] = Clause(p[1][0], p[1][1], p[3][0], p[3][1])

def p_clause_unit(p):
    'clause : unit'
    p[0] = Clause(p[1][0], p[1][1])

def p_unit_negation(p):
    'unit : NOT unit'
    p[0] = (p[2][0], True)

def p_unit_var(p):
    'unit : VARIABLE'
    p[0] = (p[1], False)

tokens = ('VARIABLE', 'AND', 'OR', 'IMPLIES', 'NOT')
literals = ['(', ')']

t_VARIABLE = r'[a-z]'
t_AND = r'/\\'
t_OR = r'\\/'
t_IMPLIES = r'->'
t_NOT = r'~'
t_ignore = ' \t'

def t_error(t): t.lexer.skip(1)
lexer = lex.lex()
parser = yacc.yacc()

class Clause:
    def __init__(self, value1, negation1=False, value2=None, negation2=False):
        self.values = [(value1, negation1), (value2 or None, negation2)]
        self._evaluate_clause_status()

    def _evaluate_clause_status(self):
        self.tautology, self.satisfiable = False, True
        if self.values[0][0] == self.values[1][0]:
            self.tautology = self.values[0][1] != self.values[1][1]
            if self.values[0][0] is None: 
                self.satisfiable = False
            else: 
                self.values[0] = (None, False)

class Expression:
    def __init__(self, clauses):
        self.clauses = clauses
        self.satisfiable = None

    def filter_and_resolve(self):
        self.clauses = [clause for clause in self.clauses if not clause.tautology]
        self.satisfiable = True
        while True:
            clauses = list(self.clauses)
            for clause_i in clauses:
                for clause_j in clauses:
                    if clause_i == clause_j: continue
                    for i in range(2):
                        for j in range(2):
                            i_val, i_neg = clause_i.values[i]
                            j_val, j_neg = clause_j.values[j]
                            if i_val == j_val and i_neg != j_neg:
                                new_clause = Clause(clause_i.values[1-i][0], clause_i.values[1-i][1], clause_j.values[1-j][0], clause_j.values[1-j][1])
                                if not new_clause.satisfiable:
                                    self.clauses.append(new_clause)
                                    self.satisfiable = False
                                    return
                                if not new_clause.tautology and not any(2 == sum(1 for i in range(2) for j in range(2) if new_clause.values[i] == clause.values[j]) for clause in self.clauses):
                                    self.clauses.append(new_clause)
            if len(clauses) == len(self.clauses):
                return self.satisfiable
        
    def sat_assignment(self):
        cnf = self 
        unused_variables = set()
        assignment = dict()
        
        for clause in cnf.clauses:
            for (value, _) in clause.values:
                if value is not None:
                    unused_variables.add(value)
        
        for combination in range(2 ** len(unused_variables)):
            temp_assignment = {var: bool(combination & (1 << index)) for index, var in enumerate(unused_variables)}
            if cnf.is_formula_satisfied_by_assignment(temp_assignment):
                return temp_assignment
        return None
    
    def is_formula_satisfied_by_assignment(self, assignment):
        return all(any((value and ((not negation and assignment[value]) or (negation and not assignment[value]))) for value, negation in clause.values) for clause in self.clauses)


def is_satisfiable(input_str):
    result = parser.parse(input_str)
    result.filter_and_resolve()
    return result.satisfiable

def sat_assignment(input_str):
    result = parser.parse(input_str)
    result.filter_and_resolve()
    return result.sat_assignment()