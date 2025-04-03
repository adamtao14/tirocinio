import click, re
from rich import print
from os import path
from pyswip import Prolog, Variable
from typing import Optional

def validate_prolog_file(file_name: str) -> bool:
    """Check if the file exists and is a valid Prolog file"""
    if not path.exists(file_name):
        print(f"Error: File '{file_name}' does not exist")
        return False
    if not file_name.endswith('.pl'):
        print("Error: File must have .pl extension")
        return False
    return True

def add_to_file(file_name, data):
    if not data.endswith('.'):
        data += '.'
    try:
        with open(file_name, 'a') as f:
            f.write(f"\n{data}\n")
        return True
    except Exception as e:
        return False
# Add a fact to the specified Prolog file
def add_fact(file_name: str, fact: str) -> bool:
    
    if add_to_file(file_name, fact):
        print("[green]Fact added![/green]")
    else:
        print("[red]Error adding fact![/red]")

# Display facts from the specified Prolog file
def display_facts(file_name: str, predicate: Optional[str] = None):

    with open(file_name, 'r') as file:
        content = file.read()
    
    # Remove comments
    content = re.sub(r'%.*', '', content)  # Line comments
    content = re.sub(r'/\*.*?\*/', '', content, flags=re.DOTALL)  # Block comments
    
    # Regex pattern that specifically excludes directives
    fact_pattern = re.compile(
        r'^\s*'                    # Start of line with optional whitespace
        r'(?!:-)'                  # Negative lookahead for :-
        r'([a-z][a-zA-Z0-9_]*)'    # Predicate name
        r'\(([^)]*)\)'             # Arguments in parentheses
        r'\s*\.\s*$',             # Ending period
        re.MULTILINE
    )
    
    facts = []
    for match in fact_pattern.finditer(content):
        pred_name, pred_args = match.groups()
        if predicate is None or predicate.lower() in pred_name.lower():
            facts.append((pred_name, pred_args))
    
    if not facts:
        filter_msg = f" matching '{predicate}'" if predicate else ""
        print(f"No proper facts{filter_msg} found in the file.")
        return
    
    print(f"Facts found ({len(facts)} total{', filtered by: ' + predicate if predicate else ''}):")
    for i, (name, args) in enumerate(facts, 1):
        print(f"{i}. {name}({args}).")   


# Display rules from a specified Prolog file
def display_rules(file_name: str, pattern: Optional[str] = None):
    with open(file_name, 'r') as file:
        content = file.read()
    
    # Remove comments (optional but recommended)
    content = re.sub(r'%.*', '', content)  # Remove line comments
    content = re.sub(r'/\*.*?\*/', '', content, flags=re.DOTALL)  # Remove block comments
    
    # Regex pattern to match Prolog rules
    rule_pattern = re.compile(
        r'^\s*([a-z][a-zA-Z0-9_]*\(.[^)]*\))'  # Head of the rule
        r'\s*:-'                               # :- operator
        r'\s*(.*?)'                             # Body of the rule
        r'\s*\.\s*$',                           # Ending period
        re.MULTILINE | re.DOTALL
    )
    
    rules = rule_pattern.findall(content)
    
    if not rules:
        print("No rules found in the file.")
        return
    
    print(f"Rules found ({len(rules)} total{', filtered by: ' + pattern if pattern else ''}):")
    for i, (head, body) in enumerate(rules, 1):
        print("\n")
        if pattern:
            if pattern in head:
                print(f"{i}. [green]{head}[/green] :- {body}.")
        else:
            print(f"{i}. [green]{head}[/green] :- {body}.")


def add_rule_to_file(file_name, in_line, from_file):
    if in_line:
        to_add = in_line
    elif from_file:
        try:
            with open(from_file, 'r') as file:
                to_add = file.read()
            print("[green]Rule added successfully![/green]")
            return True
        except Exception as e:
            print(f"[red]Error adding rule: {e}[/red]")
            return False
            
    else:
        print("[yellow]You must specify one of the options![/yellow]")
        return False
    
    return add_to_file(file_name, to_add)


@click.group()
def cli():
    """VAPT 2.0 - Vulnerability Assessment and Penetration Testing Tool"""
    pass

@cli.command()
@click.argument('file_name', type=click.Path(exists=True))
@click.argument('fact')
def add(file_name, fact):
    """Add a fact to the specified Prolog file"""
    if not validate_prolog_file(file_name):
        return
    return add_fact(file_name, fact)


@cli.command()
@click.argument('file_name', type=click.Path(exists=True))
@click.option('--predicate', '-p', help='Show specific predicate only')
def facts(file_name, predicate):
    """Display facts from the specified Prolog file"""
    if not validate_prolog_file(file_name):
        return
    display_facts(file_name, predicate)

@cli.command()
@click.argument('file_name', type=click.Path(exists=True))
@click.option('--pattern', '-p', help='Show rules containing the specified pattern')
def rules(file_name, pattern = None):
    """Display rules from the specified Prolog file"""
    if not validate_prolog_file(file_name):
        return
    display_rules(file_name, pattern)

@cli.command()
@click.argument('file_name', type=click.Path(exists=True))
@click.option('--in-line', '-i', help='Adda new rule in line')
@click.option('--from-file', '-f', type=click.Path(exists=True), help='Add rules from a specified file')
def add_rule(file_name, in_line: Optional[str] = None, from_file: Optional[str] = None):
    if not validate_prolog_file(file_name):
        return
    else:
        return add_rule_to_file(file_name,in_line,from_file)
        


if __name__ == '__main__':
    cli()