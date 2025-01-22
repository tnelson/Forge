#lang forge/domains/abac

// Fiction: the policy language doesn't support "not" in rules, similar to many basic firewalls.
// This means "deny" rules are far more important! Without them, expressive power of the language goes down.

policy original
  // Administrators can read and write anything
  permit if: s is admin, a is read.
  permit if: s is admin, a is write.
  // Files being audited can't be changed by customers
  deny   if: a is write, r is file, r is under-audit.
  // Customers have full access to files they own
  permit if: s is customer, a is read, r is owned-by s.
  permit if: s is customer, a is write, r is owned-by s.
end;

// Cloud services company. Customers store/update their data. Sometimes audits need to be performed. At first the company is small,
// and audits are facilitated by admin who freeze the file and outsource the actual audit. But then it grows, and creates a
// new auditing department.
// (application and removal of the auditing flag is outside the scope of this policy)

policy modified
  // Administrators can read and write anything
  permit if: s is admin, a is read.
  permit if: s is admin, a is write.
  // Files being audited can't be changed by customers
  deny   if: a is write, r is file, r is under-audit.
  // Customers have full access to files they own
  permit if: s is customer, a is read, r is owned-by s.
  permit if: s is customer, a is write, r is owned-by s.

  // Once completing training, accountants can read and write (for annotation) to files under audit
  deny   if: s is in-training.
  permit if: s is accountant, a is read, r is under-audit.
  permit if: s is accountant, a is write, r is under-audit.    
end;

policy modified2
  // Administrators can read and write anything
  permit if: s is admin, a is read.
  permit if: s is admin, a is write.

  // Once completing training, accountants can read and write (for annotation) to files under audit
  deny   if: s is in-training.
  permit if: s is accountant, a is read, r is under-audit.
  permit if: s is accountant, a is write, r is under-audit. 

  // Files being audited can't be changed by customers
  deny   if: a is write, r is file, r is under-audit.
  // Customers have full access to files they own
  permit if: s is customer, a is read, r is owned-by s.
  permit if: s is customer, a is write, r is owned-by s.

end;


// compare x; // error
compare original modified;
query original yields permit where s is not admin;
compare original modified2 where s is not accountant;


