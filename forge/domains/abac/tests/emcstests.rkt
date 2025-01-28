#lang forge/domains/abac

policy filesystem
  // Administrators can read and write anything
  permit if: s is admin, a is read.
  permit if: s is admin, a is write.
  // Files being audited can't be changed by customers
  deny   if: a is write, r is file, r is under-audit.
  // Customers have full access to files they own
  permit if: s is customer, a is read, r is owned-by s.
  permit if: s is customer, a is write, r is owned-by s.
end;

query filesystem yields permit where s is customer, a is read;

policy filesystem2
  // Administrators can read and write anything
  permit if: s is admin, a is read.
  permit if: s is admin, a is write.
  // Files being audited can't be changed by customers
  deny   if: a is write, r is file, r is under-audit.
  // Customers have full access to files they own
  permit if: s is customer, a is read, r is owned-by s.
  permit if: s is customer, a is write, r is owned-by s.

  // Accountants can read and write files under audit
  //   but only after completing training
  deny   if: s is in-training.
  permit if: s is accountant, a is read, r is under-audit.
  permit if: s is accountant, a is write, r is under-audit.    
end;

query filesystem2 yields permit where s is accountant, a is write, r is under-audit;
query filesystem2 yields permit where s is not admin, s is accountant, a is write, r is under-audit;
query filesystem2 yields deny where s is not admin, s is accountant, a is write, r is under-audit;


// policy for testing
policy test
  permit if: s is admin, a is read.
  deny if: true.
end;
compare test test; // should not complain about no hash-value for key 'r$0.

