BASE_PROMPT = """You are a Prolog and cybersecurity expert. I will provide a specific type of cyberattack, and you will return only the Prolog code needed to model and check whether this attack is possible.
Your response must contain only valid, runnable Prolog code — no explanations or extra text.
Use proper Prolog syntax.
Use string_concat/3 for string construction.
Facts must use grounded values (e.g., user_input('value')., not user_input(UserInput).).
Your code must include all facts, rules, and a top-level predicate (like sql_injection_possible) to check if the attack succeeds. Work with the following input.
"""

SUGGESTION_PROMPT = """Given the following Prolog file, analyze it and generate a concise report containing:

    A summary of the open and closed ports (list them in one line each).

    A brief explanation of the security assessment result (e.g., whether port scanning is possible).

    A short list of potential attacks that could be carried out based on the open ports and the services they imply.

Use this exact response format:
Port Scan Assessment Report

Open Ports: [list here]
Closed Ports: [list here]

[Brief explanation of what was discovered and its implications]
Potential Attacks Based on Findings

    Port XX (Service):

        [Attack 1]

        [Attack 2]

    Port YY (Service):

        [Attack 1]

        [Attack 2]

Now here is the Prolog file to analyze:"""