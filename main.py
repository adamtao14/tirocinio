import click, re, os, time, shutil
from random import randint
from utils import is_in_same_dir, delete_temp_file, copy_pl_file
from rich import print
from pyswip import Prolog
from typing import Optional
from openai import OpenAI
from dotenv import load_dotenv
from prompts import BASE_PROMPT, SUGGESTION_PROMPT, ANALYZE_PROMPT, EXPLOIT_PROMPT
from halo import Halo
load_dotenv()


def validate_prolog_file(file_name: str) -> bool:
    """Check if the file exists and is a valid Prolog file"""
    if not os.path.exists(file_name):
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
def display_facts(file_name: str, pattern: Optional[str] = None):

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
        if pattern is None or pattern.lower() in pred_name.lower():
            facts.append((pred_name, pred_args))
    
    if not facts:
        filter_msg = f" matching '{pattern}'" if pattern else ""
        print(f"No proper facts{filter_msg} found in the file.")
        return
    
    print(f"Facts found ({len(facts)} total{', filtered by: ' + pattern if pattern else ''}):")
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
       
        if pattern:
            if pattern in head:
                print(f"\n{i}. [green]{head}[/green] :- {body}.")
        else:
            print(f"\n{i}. [green]{head}[/green] :- {body}.")


# Display rules from a specified Prolog file
def display_predicates(file_name: str, pattern: Optional[str] = None):
    with open(file_name, 'r') as file:
        content = file.read()
    
    # Remove comments (optional but recommended)
    content = re.sub(r'%.*', '', content)  # Remove line comments
    content = re.sub(r'/\*.*?\*/', '', content, flags=re.DOTALL)  # Remove block comments
    
    # Regex pattern to match Prolog rules
    rule_pattern = re.compile(
        r'^\s*([a-z][a-zA-Z0-9_]*)'  # Head of the rule
        r'\s*:-'                               # :- operator
        r'\s*(.*?)'                             # Body of the rule
        r'\s*\.\s*$',                           # Ending period
        re.MULTILINE | re.DOTALL
    )
    
    rules = rule_pattern.findall(content)
    
    if not rules:
        print("No predicates found in the file.")
        return
    
    print(f"Predicates found ({len(rules)} total{', filtered by: ' + pattern if pattern else ''}):")
    for i, (head, body) in enumerate(rules, 1):
       
        if pattern:
            if pattern in head:
                print(f"\n{i}. [green]{head}[/green] :- {body}.")
        else:
            print(f"\n{i}. [green]{head}[/green] :- {body}.")

# Add a rule to the specified Prolog file
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

# Delete a specific fact from a Prolog file
def delete_fact_from_file(file_name, content):
    lines = []
    with open(file_name, 'r') as file:
        lines = file.readlines()
    target_lines = content.splitlines()
    found_at_least_once = False
    result = []
    i = 0
    while i < len(lines):
        if lines[i].strip("\n") == target_lines[0].strip("\n"):
            match = True
            for j in range(1, len(target_lines)):
                if i+j >= len(lines) or lines[i+j] != target_lines[j].strip("\n"):
                    match = False
                    break
            if match:
                i += len(target_lines)
                found_at_least_once = True
                continue
        result.append(lines[i])
        i += 1
    
    output = ''.join(result)
    if found_at_least_once:
        print("[green]Fact eliminated successfully![/green]")
    else:
        print("[red]Fact was not found in the file![/red]")
    try:    
        with open(file_name,'w') as output_file:
            output_file.write(output)
    except Exception as e:
        print("[red]Failed to write to file[/red]")
    return True



# Query the specified Prolog file with the given query
def query_prolog_file(file_name, query):
    if query == "" or query is None:
        print("[red]You must specify a query![/red]")
        return False
    try:
        spinner = Halo(text='Loading\n', spinner='dots')
        spinner.start()
        prolog = Prolog()
        in_same_dir = is_in_same_dir(os.getcwd(), file_name)
        if not in_same_dir:
            new_path = copy_pl_file(file_name)
        else:
            new_path = file_name
        prolog.consult(os.path.basename(new_path))
        

        if not query.endswith('.'):
            query += '.'
        
        results = list(prolog.query(query))
        spinner.stop()
        if not results:
            print("[red]Query returned no results.[/red]")
            return False
        
        print(f"Results for query '{query}':")
        
        for i, result in enumerate(results, 1):
            print(f"\nResult {i}:")
            for key, value in result.items():
                print(f"  {key}: ", end="")
                print_prolog_value(value)
        if not in_same_dir:
            delete_temp_file(new_path)
        return True        
    except Exception as e:
        if not in_same_dir:
            delete_temp_file(new_path)
        print(f"Error executing query: {str(e)}")
        return False


def print_prolog_value(value, indent=2):
    """Recursively prints Prolog values in a readable format"""
    indent_str = ' ' * indent
    
    if isinstance(value, bytes):
        print(value.decode('utf-8'))
    elif isinstance(value, list):
        if not value:
            print("[]")
        else:
            print()
            for item in value:
                print(f"{indent_str}- ", end="")
                print_prolog_value(item, indent + 2)
    elif isinstance(value, dict):
        print()
        for k, v in value.items():
            print(f"{indent_str}{k}: ", end="")
            print_prolog_value(v, indent + 2)
    else:
        print(value)

# Generate via OpenAI's API a specific Prolog file for a given vulenerability attack
def llm_generation(user_input, prompt):
    client = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))
    spinner = Halo(text='Generating response', spinner='dots')
    spinner.start()
    start_time = time.time()
    response = client.responses.create(
        model="gpt-4.1",
        input=prompt + "\n" + user_input
    )
    end_time = time.time()
    elapsed_time = round(end_time - start_time, 3)
    

    if len(user_input) <= 20:
        file_name = user_input.replace(' ', '_')
    else:
        file_name = user_input[:20].replace(' ', '_')

    # if the file_name already exists, add a random number to the file_name
    if os.path.exists(file_name + ".pl"):
        file_name += "_" + str(randint(1,100))
    
    file_name += ".pl"
    try:
        with open(file_name, "w") as f:
            # remove first and last line from response.output_text because of response style
            # ```prolog .... ```    
            f.write(response.output_text[9:-3])
    except Exception as e:
        spinner.stop()
        print("[red]Failed to write to file[/red]")
    
    spinner.stop()
    print(f"[green]Generated {file_name}[/green]")
    print(f"Elapsed time: {elapsed_time}s\n")
    return True
# Get a report of what can be analysed from a single or a list of Prolog files
def suggest_from_files(file_names, output_markdown):
    client = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))
    combined_output = ""
    
    # Read and combine all files
    for file_name in file_names:
        with open(file_name, "r") as f:
            text = f.readlines()
            combined_output += ' '.join(text) + "\n\n"  # Separate files with blank lines
    
    if combined_output.strip() == "":
        print("[red]All provided Prolog files are empty![/red]")
        return False
    
    spinner = Halo(text='Generating response', spinner='dots')
    spinner.start()
    
    start_time = time.time()
    
    try:
        response = client.responses.create(
            model="gpt-4.1",
            input=SUGGESTION_PROMPT + "\n" + combined_output
        )
    except Exception as e:
        spinner.stop()
        print(f"[red]Error generating response: {str(e)}[/red]")
        return False
    
    end_time = time.time()
    elapsed_time = round(end_time - start_time, 3)
    
    spinner.stop()
    if output_markdown != "":
        try:
            with open(output_markdown, 'w') as f:
                f.write(response.output_text)
        except Exception as e:
            print("[red]Failed to write to file[/red]")
        
        print(f"[green]Response saved in {output_markdown}[/green]")
        print(f"Elapsed time: {elapsed_time}s")
    else:
        print(f"[green]Response generated[/green]")
        print(f"Elapsed time: {elapsed_time}s")
        print("-----------------\n")
        print(response.output_text)
    return True

# COMMANDS

@click.group()
def cli():
    """VAPT 2.0 - Vulnerability Assessment and Penetration Testing Tool"""
    pass

@cli.command()
@click.argument('file_name', type=click.Path(exists=True))
@click.argument('fact')
def add(file_name, fact):
    if not validate_prolog_file(file_name):
        return
    return add_fact(file_name, fact)

@cli.command()
@click.argument('file_name', type=click.Path(exists=True))
@click.option('--pattern', '-p', help='Show specific pattern only')
def predicates(file_name, pattern):
    if not validate_prolog_file(file_name):
        return
    display_predicates(file_name, pattern)


@cli.command()
@click.argument('file_name', type=click.Path(exists=True))
@click.option('--pattern', '-p', help='Show specific pattern only')
def facts(file_name, pattern):
    if not validate_prolog_file(file_name):
        return
    display_facts(file_name, pattern)

@cli.command()
@click.argument('file_name', type=click.Path(exists=True))
@click.option('--pattern', '-p', help='Show rules containing the specified pattern')
def rules(file_name, pattern = None):
    if not validate_prolog_file(file_name):
        return
    display_rules(file_name, pattern)

@cli.command()
@click.argument('file_name', type=click.Path(exists=True))
@click.option('--in-line', '-i', help='Add a new rule in line')
@click.option('--from-file', '-f', type=click.Path(exists=True), help='Add rules from a specified file')
def add_rule(file_name, in_line: Optional[str] = None, from_file: Optional[str] = None):
    if not validate_prolog_file(file_name):
        return
    else:
        return add_rule_to_file(file_name,in_line,from_file)
        
@cli.command()
@click.argument('file_name', type=click.Path(exists=True))
@click.argument('fact')
def delete_fact(file_name, fact):
    if not validate_prolog_file(file_name):
        return
    return delete_fact_from_file(file_name,fact)


@cli.command()
@click.argument('file_name', type=click.Path(exists=True))
@click.argument('query')
def query(file_name, query):
    if not validate_prolog_file(file_name):
        return
    return query_prolog_file(file_name, query)

@cli.command()
@click.argument('input')
def gen_ai(input = ""):
    if input == "":
        return
    else:
        return llm_generation(input, SUGGESTION_PROMPT)

@cli.command()
@click.argument('file_names', type=click.Path(exists=True), nargs=-1)
@click.option('--output-markdown', '-o', type=click.Path(), help="Save the report to a Markdown file")
def suggest(file_names, output_markdown):
    valid_files = [f for f in file_names if validate_prolog_file(f)]
    if not valid_files:
        click.echo("No valid Prolog files provided")
        return
    return suggest_from_files(valid_files, output_markdown)

@cli.command()
@click.argument('input')
def analyze(input):
    if input == "":
        return
    else:
        return llm_generation(input, ANALYZE_PROMPT)

@cli.command()
@click.argument('input')
def exploit(input):
    if input == "":
        return
    else:
        return llm_generation(input, EXPLOIT_PROMPT)



if __name__ == '__main__':
    cli()