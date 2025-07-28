///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <string_view>


namespace Langulus
{
   using Token = ::std::string_view;

   /// A fully portable constexpr alphabetical character check                
   /// Only english alphabet and underline symbol are allowed                 
   // ReSharper disable once CppDFAUnreachableFunctionCall
   constexpr bool IsAlphabetical(char c) noexcept {
      switch (c) {
      case 'A': case 'a': case 'B': case 'b': case 'C': case 'c':
      case 'D': case 'd': case 'E': case 'e': case 'F': case 'f':
      case 'G': case 'g': case 'H': case 'h': case 'I': case 'i':
      case 'J': case 'j': case 'K': case 'k': case 'L': case 'l':
      case 'M': case 'm': case 'N': case 'n': case 'O': case 'o':
      case 'P': case 'p': case 'Q': case 'q': case 'R': case 'r':
      case 'S': case 's': case 'T': case 't': case 'U': case 'u':
      case 'V': case 'v': case 'W': case 'w': case 'X': case 'x':
      case 'Y': case 'y': case 'Z': case 'z': case '_':
         return true;
      default:
         return false;
      }
   }

   /// A fully portable constexpr lowercase character check                   
   // ReSharper disable once CppDFAUnreachableFunctionCall
   constexpr bool IsLowercase(char c) noexcept {
      return c != '_' and IsAlphabetical(c) and c >= 'a';
   }

   /// A fully portable constexpr lowercase character check                   
   // ReSharper disable once CppDFAUnreachableFunctionCall
   constexpr bool IsUppercase(char c) noexcept {
      return c != '_' and IsAlphabetical(c) and c <= 'Z';
   }

   /// A fully portable constexpr operator character check                    
   // ReSharper disable once CppDFAUnreachableFunctionCall
   constexpr bool IsOperator(char c) noexcept {
      switch (c) {
      case '<': case '>': case '[': case ']': case '(': case ')':
      case '*': case '&': case ':': case ';': case '"': case '\'':
      case '.': case ',': case '{': case '}': case '+': case '-':
      case '/': case '?': case '!': case '@': case '#': case '$':
      case '%': case '^': case '=': case '`': case '\\': case '|':
         return true;
      default:
         return false;
      }
   }

   /// A fully portable constexpr number character check                      
   // ReSharper disable once CppDFAUnreachableFunctionCall
   constexpr bool IsNumerical(char c) noexcept {
      switch (c) {
      case '0': case '1': case '2': case '3': case '4': case '5':
      case '6': case '7': case '8': case '9':
         return true;
      default:
         return false;
      }
   }
   
   /// A fully portable constexpr space character check                       
   // ReSharper disable once CppDFAUnreachableFunctionCall
   constexpr bool IsSpace(char c) noexcept {
      return c == ' ';
   }

   /// Verify that a string literal is made of allowed ASCII symbols          
   constexpr bool IsASCII(const Token& source) noexcept {
      for (char c : source) {
         if (IsAlphabetical(c) or IsOperator(c) or IsNumerical(c) or IsSpace(c))
            continue;
         return false;
      }
      return true;
   }

   /// A fully portable constexpr lowercase character                         
   constexpr char ToLowercase(char c) noexcept {
      switch (c) {
      case 'A': return 'a';
      case 'B': return 'b';
      case 'C': return 'c';
      case 'D': return 'd';
      case 'E': return 'e';
      case 'F': return 'f';
      case 'G': return 'g';
      case 'H': return 'h';
      case 'I': return 'i';
      case 'J': return 'j';
      case 'K': return 'k';
      case 'L': return 'l';
      case 'M': return 'm';
      case 'N': return 'n';
      case 'O': return 'o';
      case 'P': return 'p';
      case 'Q': return 'q';
      case 'R': return 'r';
      case 'S': return 's';
      case 'T': return 't';
      case 'U': return 'u';
      case 'V': return 'v';
      case 'W': return 'w';
      case 'X': return 'x';
      case 'Y': return 'y';
      case 'Z': return 'z';
      default: return c;
      }
   }
   /// A fully portable constexpr uppercase character                         
   constexpr char ToUppercase(char c) noexcept {
      switch (c) {
      case 'a': return 'A';
      case 'b': return 'B';
      case 'c': return 'C';
      case 'd': return 'D';
      case 'e': return 'E';
      case 'f': return 'F';
      case 'g': return 'G';
      case 'h': return 'H';
      case 'i': return 'I';
      case 'j': return 'J';
      case 'k': return 'K';
      case 'l': return 'L';
      case 'm': return 'M';
      case 'n': return 'N';
      case 'o': return 'O';
      case 'p': return 'P';
      case 'q': return 'Q';
      case 'r': return 'R';
      case 's': return 'S';
      case 't': return 'T';
      case 'u': return 'U';
      case 'v': return 'V';
      case 'w': return 'W';
      case 'x': return 'X';
      case 'y': return 'Y';
      case 'z': return 'Z';
      default: return c;
      }
   }
   
   /// Check if a token transition happens at the beginning and the end of    
   /// a region inside a source. A token transition means, that the token     
   /// is surrounded by non-alphabetical symbols                              
   ///   @param source - data source                                          
   ///   @param lhs - start of the region                                     
   ///   @param rhs - end of the region                                       
   ///   @return true if a transition occurs at both points                   
   // ReSharper disable once CppDFAUnreachableFunctionCall              
   constexpr bool IsTransition(const Token& source, size_t lhs, size_t rhs) noexcept {
      return (
            // Test left side for transition                            
            lhs == 0
            or not IsAlphabetical(source[lhs])
            or     IsAlphabetical(source[lhs]) != IsAlphabetical(source[lhs-1])
         ) and (
            // Test right side for transition                           
            rhs >= source.size()
            or not IsAlphabetical(source[rhs-1])
            or     IsAlphabetical(source[rhs-1]) != IsAlphabetical(source[rhs])
         );
   }

   /// Check if a token satisfies all requirements for being a keyword:       
   /// 1. Must be a continuous string of ASCII characters, no spaces          
   /// 2. Must start with an alphabetical symbol                              
   /// 3. Must not contain any operators, except <>:,                         
   ///   @param token - the token to check                                    
   ///   @return true if token is a valid keyword                             
   constexpr bool IsKeyword(const Token& token) noexcept {
      if (token.empty() or not IsAlphabetical(token[0]))
         return false;

      int template_depth = 0;
      int namespace_separator = 0;
      for (char c : token) {
         if (not IsAlphabetical(c) and not IsNumerical(c)) {
            switch (c) {
            case '<':
               ++template_depth;
               break;
            case '>':
               --template_depth;
               break;
            case ':':
               ++namespace_separator;
               if (namespace_separator > 2)
                  return false;
               break;
            default:
               return false;
            }
         }
         else namespace_separator = 0;
      }
      return template_depth == 0;
   }
}
