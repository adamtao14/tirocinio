import os

def normalize_path_for_prolog(file_path):
    abs_path = os.path.abspath(os.path.normpath(file_path))
    prolog_path = abs_path.replace('\\', '/')
    # Step 3: Handle Unicode characters by doubling backslashes
    # This is the key fix for PySwip's Prolog interface
    prolog_consult_path = abs_path.replace('\\', '\\\\')
    
    return abs_path, prolog_consult_path
