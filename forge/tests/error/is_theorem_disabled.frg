#lang forge

// Expect an error here, since "is theorem" is disabled in favor of "is checked"
test expect { 
  {} is theorem
}
