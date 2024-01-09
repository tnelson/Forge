#lang forge/domains/abac

// Remember semicolons!

policy empty
  permit if: true.  
end;

policy singleCondition
  permit if: s is admin.
end;

policy original
  permit if: s is admin, a is read, r is file.
end;

policy mod1
  permit if: s is admin, a is read, r is file.
  permit if: s is accountant, a is read, r is under-audit.
end;

info;

compare original mod1;

//test original s is admin, a is read, r is under-audit;
//test original s is not admin, a is read, r is under-audit;
//test mod1     s is not admin, a is read, r is under-audit;