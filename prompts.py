BASE_PROMPT = """You are a Prolog and cybersecurity expert. I will provide a specific type of cyberattack, and you will return only the Prolog code needed to model and check whether this attack is possible.
Your response must contain only valid, runnable Prolog code — no explanations or extra text.
Use proper Prolog syntax.
Use string_concat/3 for string construction.
Facts must use grounded values (e.g., user_input('value')., not user_input(UserInput).).
Your code must include all facts, rules, and a top-level predicate (like sql_injection_possible) to check if the attack succeeds. Work with the following input.
"""

SUGGESTION_PROMPT = """Analyze the following Prolog knowledge base and generate a structured security report in markdown format.  
Focus only on what can be inferred from the facts. Omit sections where no relevant data exists.

### Report Structure (Include Only What Applies):

1. **Key Observations**  
   - Summarize the main findings (e.g., open ports, vulnerabilities, attacker behavior).  
   - Example: "The file describes 3 open ports and a brute-force attack pattern."

2. **Network Exposure** (if ports/services are mentioned)  
   - Open Ports: [list if `port(open, X, Service)` facts exist]  
   - Closed Ports: [list if `port(closed, X, _)` facts exist]  

3. **Security Implications**  
   - Briefly explain risks (e.g., "Port 22 (SSH) allows brute-force attempts").  
   - Skip if no actionable findings.  

4. **Potential Threats** (if evidence exists)  
   - For ports: List attacks relevant to their services (e.g., "SSH → brute-force").  
   - For vulnerabilities: Link to possible exploits (e.g., "Heartbleed → data leakage").  
   - For attack patterns: Describe scenarios (e.g., "IP 1.2.3.4 performed scanning").  

5. **Recommendations** (if risks are found)  
   - Suggest mitigations (e.g., "Close port 23/telnet").  

### Input to Analyze:"""