import os,shutil

def normalize_path_for_prolog(file_path):
    abs_path = os.path.abspath(os.path.normpath(file_path))
    prolog_path = abs_path.replace('\\', '/')
    # Step 3: Handle Unicode characters by doubling backslashes
    # This is the key fix for PySwip's Prolog interface
    prolog_consult_path = abs_path.replace('\\', '\\\\')
    
    return abs_path, prolog_consult_path

def is_in_same_dir(current_dir, file_path):
    abs_path = os.path.abspath(os.path.normpath(file_path))
    return current_dir == os.path.dirname(abs_path)

def copy_pl_file(file_to_move):
    current_dir = os.getcwd()
    abs_path = os.path.abspath(os.path.normpath(file_to_move))
    file_name = os.path.basename(abs_path)
    dest = f"{current_dir}\\{file_name}"
    shutil.copy(abs_path, dest)
    return dest

def delete_temp_file(file_path):
    if os.path.exists(file_path):
        os.remove(file_path)   
    else:
        print(f"{file_path} does not exist") 