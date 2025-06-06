import ast


    
    
with open("/home/ericr/dev/Python-Static-Type-Checker/Testing_Python/input.txt", "r") as file:
    file_content = file.read()
    tree = ast.parse(file_content)
    print(ast.dump(tree, indent=4))

def f():
    pass

