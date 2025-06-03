///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Literal.hpp"
#include "CT/Named.hpp"


namespace Langulus::RTTI
{

   using Lowercase = ::std::string;

   namespace Inner
   {

      /// A fully portable constexpr alphabetical character check             
      /// Only english alphabet and underline symbol are allowed              
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

      /// A fully portable constexpr operator character check                 
      /// Only operators that can occur in type names are allowed             
      constexpr bool IsOperator(char c) noexcept {
         switch (c) {
         case '<': case '>': case '[': case ']': case '(': case ')':
         case '*': case '&': case ':': case ';': case '"': case '\'':
         case '.': case ',':
            return true;
         default:
            return false;
         }
      }

      /// A fully portable constexpr number character check                   
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
      constexpr bool IsSpace(char c) noexcept {
         return c == ' ';
      }

      /// Verify that a string literal is made of allowed ASCII symbols       
      constexpr bool IsASCII(auto source) {
         for (char c : source) {
            if (IsAlphabetical(c) or IsOperator(c) or IsNumerical(c) or IsSpace(c))
               continue;
            return false;
         }
         return true;
      }

      /// Types used for pattern matching while isolating typenames           
      /// These need to be in exactly this namespace to avoid corner cases    
      struct Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK {};
      enum { Oddly_Specific_EnumASDOLSAJDPAFHOAF };

      /// Stringifies type T by exploiting the preprocessor                   
      template<class T>
      consteval auto WrappedTypeName() {
         return Literal {LANGULUS_FUNCTION()};
      }

      /// Stringifies value T by exploiting the preprocessor                  
      template<auto T>
      consteval auto WrappedEnumName() {
         return Literal {LANGULUS_FUNCTION()};
      }

      /// Analyze compiler stringification and find the left offset in order  
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the left           
      consteval auto CalculateTypeLeftOffset() {
         constexpr auto calibration_name = 
            WrappedTypeName<Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK>();
         constexpr auto start1 = calibration_name.find(
            "Langulus::RTTI::Inner::Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK");

         if constexpr (start1 != calibration_name.npos)
            return start1;
         else {
            // Most compilers include the namespaces. GCC14 in          
            // particular decided not to...                             
            constexpr auto start2 = calibration_name.find(
               "Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK");

            static_assert(start2 != calibration_name.npos);
            return start2;
         }
      }
      constexpr auto CalibratedTypeLeftOffset = CalculateTypeLeftOffset();
      
      /// Analyze compiler stringification and find the right offset in order 
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the right          
      consteval auto CalculateTypeRightOffset() {
         constexpr auto calibration_name = 
            WrappedTypeName<Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK>();
         constexpr auto start1 = calibration_name.find(
            "Langulus::RTTI::Inner::Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK");

         if constexpr (start1 != calibration_name.npos)
            return calibration_name.size() - start1 - 61;
         else {
            // Most compilers include the namespaces. GCC14 in          
            // particular decided not to...                             
            constexpr auto start2 = calibration_name.find(
               "Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK");

            static_assert(start2 != calibration_name.npos);
            return calibration_name.size() - start2 - 38;
         }
      }
      constexpr auto CalibratedTypeRightOffset = CalculateTypeRightOffset();

      /// Analyze compiler stringification and find the left offset in order  
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the left           
      consteval auto CalculateEnumLeftOffset() {
         constexpr auto calibration_name = 
            WrappedEnumName<Oddly_Specific_EnumASDOLSAJDPAFHOAF>();
         constexpr auto start1 = calibration_name.find(
            "Langulus::RTTI::Inner::Oddly_Specific_EnumASDOLSAJDPAFHOAF");

         if constexpr (start1 != calibration_name.npos)
            return start1;
         else {
            // Most compilers include the namespaces. GCC14 in          
            // particular decided not to...                             
            constexpr auto start2 = calibration_name.find(
               "Oddly_Specific_EnumASDOLSAJDPAFHOAF");

            static_assert(start2 != calibration_name.npos);
            return start2;
         }
      }
      constexpr auto CalibratedEnumLeftOffset = CalculateEnumLeftOffset();

      /// Analyze compiler stringification and find the right offset in order 
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the right          
      consteval auto CalculateEnumRightOffset() {
         constexpr auto calibration_name = 
            WrappedEnumName<Oddly_Specific_EnumASDOLSAJDPAFHOAF>();
         constexpr auto start1 = calibration_name.find(
            "Langulus::RTTI::Inner::Oddly_Specific_EnumASDOLSAJDPAFHOAF");

         if constexpr (start1 != calibration_name.npos)
            return calibration_name.size() - start1 - 58;
         else {
            // Most compilers include the namespaces. GCC14 in          
            // particular decided not to...                             
            constexpr auto start2 = calibration_name.find(
               "Oddly_Specific_EnumASDOLSAJDPAFHOAF");

            static_assert(start2 != calibration_name.npos);
            return calibration_name.size() - start2 - 35;
         }
      }
      constexpr auto CalibratedEnumRightOffset = CalculateEnumRightOffset();

      /// Skip all decorations in front and the back of a WrappedTypeName     
      ///   @return the type name                                             
      template<class T>
      consteval auto IsolateTypename() {
         constexpr auto name = WrappedTypeName<T>();
         constexpr auto size = name.size();
         constexpr auto left = CalibratedTypeLeftOffset;
         constexpr auto right = CalibratedTypeRightOffset;
         static_assert(size > left + right, "Invalid type name");
         return name.template substr<left, size - right - left>();
      }

      /// Skip all decorations in front and back of a WrappedEnumName         
      ///   @return the enum name                                             
      template<auto T>
      consteval auto IsolateConstant() {
         constexpr auto name  = WrappedEnumName<T>();
         constexpr auto size  = name.size();
         constexpr auto left  = CalibratedEnumLeftOffset;
         constexpr auto right = CalibratedEnumRightOffset;
         static_assert(size > left + right, "Invalid enum name");
         return name.template substr<left, size - right - left>();
      }

      /// Check if a token transition happens at the beginning and the end of 
      /// a region inside a source. A token transition means, that the token  
      /// is surrounded by non-alphabetical symbols                           
      ///   @param source - data source                                       
      ///   @param lhs - start of the region                                  
      ///   @param rhs - end of the region                                    
      ///   @return true if a transition occurs at both points                
      constexpr bool IsTransition(auto source, std::size_t lhs, std::size_t rhs) {
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
      
      /// Count the number of found tokens, if separated by non-alphabetical  
      /// symbols                                                             
      ///   @tparam LHS - what are we checking?                               
      ///   @tparam RHS - what are we searching for?                          
      template<Literal LHS, Literal RHS>
      consteval std::size_t CountOccurences() {
         if constexpr (RHS.size() > LHS.size() or RHS.size() == 0)
            return 0;
         else {
            ::std::size_t occurences = 0;
            ::std::size_t cookie = 0;
            while (cookie + RHS.size() < LHS.size()) {
               ::std::size_t scan = 0;
               while (LHS[cookie + scan] == RHS[scan])
                  ++scan;
               
               if (scan != RHS.size()) {
                  ++cookie;
                  continue;
               }

               if (IsTransition(LHS, cookie, cookie + RHS.size())) {
                  cookie += RHS.size();
                  ++occurences;
               }
            }
            return occurences;
         }
      }

      /// Replace all occurences of a substring at compile-time               
      ///   @tparam SOURCE - what are we checking?                            
      ///   @tparam WHAT - what are we replacing?                             
      ///   @tparam WITH - what are we replacing with?                        
      ///   @return new literal                                               
      template<Literal SOURCE, Literal WHAT, Literal WITH>
      consteval auto Replace() {
         constexpr auto found = CountOccurences<SOURCE, WHAT>();
         if constexpr (not found)
            return SOURCE;
         else {
            Literal<char, SOURCE.size() - found*WHAT.size() + found*WITH.size()> result;
            std::size_t fill = 0;
            std::size_t prev = 0;
            std::size_t curr = 0;
            while ((curr = SOURCE.find(WHAT, prev)) != SOURCE.npos) {
               while (curr > prev) {
                  // Copy anything we've skipped                        
                  result[fill++] = SOURCE[prev++];
               }

               if (IsTransition(SOURCE, curr, curr + WHAT.size())) {
                  // Replace                                            
                  for (auto& c : WITH)
                     result[fill++] = c;
                  prev += WHAT.size();
               }
            }

            while (prev < SOURCE.size()) {
               // Copy any remaining trailing data                      
               result[fill++] = SOURCE[prev++];
            }

            return result;
         }
      }
      
      /// Normalize a type/enum/function name                                 
      ///   @tparam SRC - the literal to normalize                            
      ///   @return new literal                                               
      template<Literal SRC>
      consteval auto Normalize() {
         // Replace these patterns when normalizing names               
         // @attention when having similar tokens to replace, order     
         //    them correctly, with longer ones replaced first          
         // @attention replacement will not commence, if IsTransition   
         //    isn't satisifed                                          
         constexpr auto a01 = Replace<SRC, Literal {"*const "     },    Literal {"* const"}>();
         constexpr auto a02 = Replace<a01, Literal {" *const"     },    Literal {"* const"}>();
         constexpr auto a03 = Replace<a02, Literal {" *"          },    Literal {"*"      }>();
         constexpr auto a04 = Replace<a03, Literal {" &"          },    Literal {"&"      }>();
         constexpr auto a05 = Replace<a04, Literal {" >"          },    Literal {">"      }>();
         constexpr auto a06 = Replace<a05, IsolateTypename<int8_t>(),   Literal {"int8"   }>();
         constexpr auto a07 = Replace<a06, IsolateTypename<int16_t>(),  Literal {"int16"  }>();
         constexpr auto a08 = Replace<a07, IsolateTypename<int32_t>(),  Literal {"int32"  }>();
         constexpr auto a09 = Replace<a08, IsolateTypename<int64_t>(),  Literal {"int64"  }>();
         constexpr auto a10 = Replace<a09, IsolateTypename<uint8_t>(),  Literal {"uint8"  }>();
         constexpr auto a11 = Replace<a10, IsolateTypename<uint16_t>(), Literal {"uint16" }>();
         constexpr auto a12 = Replace<a11, IsolateTypename<uint32_t>(), Literal {"uint32" }>();
         constexpr auto a13 = Replace<a12, IsolateTypename<uint64_t>(), Literal {"uint64" }>();
         constexpr auto a14 = Replace<a13, Literal {"class "      },    Literal {""       }>();
         constexpr auto a15 = Replace<a14, Literal {"struct "     },    Literal {""       }>();
         constexpr auto a16 = Replace<a15, Literal {"enum "       },    Literal {""       }>();
         constexpr auto a17 = Replace<a16, Literal {"Langulus::"  },    Literal {""       }>();
         constexpr auto a18 = Replace<a17, Literal {"(__cdecl *)" },    Literal {""       }>();
         constexpr auto a19 = Replace<a18, Literal {" (*)"        },    Literal {""       }>();
         return a19;
      }

      /// Get the normalized name of a function                               
      ///   @return the normalized token for T at compile-time                
      template<class T>
      consteval auto NameOfFunction() {
         constexpr auto name = Normalize<IsolateTypename<T>()>();
         static_assert(IsASCII(name), "Function signature contains disallowed symbols");
         return "Function<" + name + ">*";
      }
      
      /// Get the normalized name of a type                                   
      ///   @return the normalized token for T at compile-time                
      template<class T>
      consteval auto NameOfType() {
         constexpr auto name = Normalize<IsolateTypename<T>()>();
         static_assert(IsASCII(name), "Type name contains disallowed symbols");
         return name;
      }
      
      /// Get the normalized name of a constant                               
      ///   @return the normalized token for T at compile-time                
      template<auto T>
      consteval auto NameOfConstant() {
         constexpr auto name = IsolateConstant<T>();
         static_assert(IsASCII(name), "Constant name contains disallowed symbols");
         constexpr auto fullEnumName = Normalize<name>();
         constexpr auto lastNamespace = fullEnumName.find_last_of(':');
         if constexpr (lastNamespace != fullEnumName.npos) {
            constexpr auto lastEnumName = fullEnumName.substr(lastNamespace);
            constexpr auto typeName = NameOfType<decltype(T)>();
            return typeName + "::" + lastEnumName;
         }
         else {
            constexpr auto typeName = NameOfType<decltype(T)>();
            return typeName + "::" + fullEnumName;
         }
      }

      /// Get the last, most relevant part of a token that may or may not     
      /// have namespaces in it. Essentially finds last "::" that isn't       
      /// enclosed in a <template>, and skip forward to that                  
      ///   @param token - the token to scan                                  
      ///   @return the last token                                            
      constexpr ::std::size_t FindLastToken(const Token& token) noexcept {
         ::std::size_t depth = 0;
         for (::std::size_t i = token.size() - 1; i < token.size(); --i) {
            switch (token[i]) {
            case ':':
               // If no depth, then we found it                         
               if (not depth)
                  return i + 1;
               break;
            case '>':
               // Open template scope                                   
               ++depth;
               break;
            case '<':
               // Close template scope                                  
               if (depth)
                  --depth;
               break;
            default:
               break;
            }
         }
         return 0;
      }

   } // namespace Langulus::RTTI::Inner
} // namespace Langulus::RTTI

namespace Langulus
{
   
   /// Get the name of a type, templated or not, with consistently named      
   /// template arguments, even if nested, at compile-time                    
   ///   @tparam T - the type to get the name of                              
   ///   @return a compile-time string                                        
   template<class T>
   consteval auto CppNameOf() {
      if constexpr (::std::is_function_v<Decay<T>>)
         return RTTI::Inner::NameOfFunction<T>();
      else
         return RTTI::Inner::NameOfType<T>();
   }
   
   /// Same as NameOf, but removes all namespaces at compile-time             
   ///   @tparam T - the type to get the name of                              
   ///   @return a compile-time string                                        
   template<class T>
   consteval auto LastCppNameOf() {
      if constexpr (::std::is_function_v<Decay<T>>)
         return RTTI::Inner::NameOfFunction<T>();
      else {
         // Find the last ':' symbol, that is not inside <...> scope    
         auto fullName = RTTI::Inner::NameOfType<T>();
         auto lastName = RTTI::Inner::FindLastToken(fullName);
         return fullName.substr(lastName);
      }
   }

   /// Get the name of an enum value at compile-time                          
   ///   @tparam E - the constant to get the name of                          
   ///   @return a compile-time string                                        
   template<auto E>
   consteval auto CppNameOf() {
      return RTTI::Inner::NameOfConstant<E>();
   }
   

   /// Same as CppNameOf, but removes all namespaces at compile-time          
   ///   @tparam T - the enum to get the name of                              
   ///   @return a compile-time string                                        
   template<auto E>
   consteval auto LastCppNameOf() {
      // Find the last ':' symbol, that is not inside <...> scope       
      auto fullName = RTTI::Inner::NameOfConstant<E>();
      auto lastName = RTTI::Inner::FindLastToken(fullName);
      return fullName.substr(lastName);
   }

   /// Get the name of a type at compile-time                                 
   /// Considers CTTI::Named, or fallbacks to the C++ name                    
   /// If you want to avoid custom names, use CppNameOf directly instead      
   ///   @attention similarly named types in anonymous namespaces will result 
   ///      in the same name. If this is not desired, give them unique        
   ///      `using CTTI_Named = YesText<"name">` for each translation unit    
   ///      they appear in manually. Alternatively, you can also specialize   
   ///      CTTI::Named instead, if you have no control over the types        
   ///   @tparam T - the type to get the name of                              
   ///   @return a compile-time string                                        
   template<class T>
   consteval auto NameOf() {
      if constexpr (CT::Named<T>) {
         if constexpr (CTTI::Named<T>::Value)
            // Checked externally, T doesn't have to be complete        
            return CTTI::Named<T>::Name;
         else if constexpr (requires { T::CTTI_Named::Value; })
            // Checked internally, T has to be complete                 
            return T::CTTI_Named::Constant;
         else
            static_assert(false, "Type improperly named");
      }
      else return CppNameOf<T>();
   }
   
   /// Get the name of an enum value at compile-time                          
   ///   @attention similarly named values in anonymous namespaces will result
   ///      in the same name. If this is not desired, specialize CTTI::Named  
   ///      for each translation unit they appear in manually                 
   ///   @tparam E - the value to get the name of                             
   ///   @return a compile-time string                                        
   template<auto E>
   consteval auto NameOf() {
      if constexpr (CT::NamedValue<E>)
         return CTTI::NamedValue<E>::Name;
      else
         return CppNameOf<E>();
   }

} // namespace Langulus
