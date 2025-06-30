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


namespace Langulus::CTTI
{

   ///                                                                        
   /// The following are some manual overrides that make stuff consistent     
   /// across different compilers                                             
   ///                                                                        
   template<>
   struct Named<::std::nullptr_t> {
      static constexpr Literal Name = "null";
      static constexpr bool Enabled = true;
   };

   template<>
   struct Named<int8_t> {
      static constexpr Literal Name = "int8";
      static constexpr bool Enabled = true;
   };

   template<>
   struct Named<int16_t> {
      static constexpr Literal Name = "int16";
      static constexpr bool Enabled = true;
   };

   template<>
   struct Named<int32_t> {
      static constexpr Literal Name = "int32";
      static constexpr bool Enabled = true;
   };

   template<>
   struct Named<int64_t> {
      static constexpr Literal Name = "int64";
      static constexpr bool Enabled = true;
   };

   template<>
   struct Named<uint8_t> {
      static constexpr Literal Name = "uint8";
      static constexpr bool Enabled = true;
   };

   template<>
   struct Named<uint16_t> {
      static constexpr Literal Name = "uint16";
      static constexpr bool Enabled = true;
   };

   template<>
   struct Named<uint32_t> {
      static constexpr Literal Name = "uint32";
      static constexpr bool Enabled = true;
   };

   template<>
   struct Named<uint64_t> {
      static constexpr Literal Name = "uint64";
      static constexpr bool Enabled = true;
   };

} // namespace Langulus::CTTI

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
      class Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK {};
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
      consteval size_t CalculateTypeLeftOffset() {
         constexpr auto calibration_name
            = WrappedTypeName<Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK>();

         #if LANGULUS_COMPILER(MSVC)
            // MSVC prepends "class "                                   
            constexpr size_t start = calibration_name.find(
               "class Langulus::RTTI::Inner::Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK");
         #elif LANGULUS_COMPILER(GCC)
            // Most compilers include the namespaces. GCC14 in          
            // particular decided not to...                             
            constexpr size_t start = calibration_name.find(
               "Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK");
         #else
            constexpr size_t start = calibration_name.find(
               "Langulus::RTTI::Inner::Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK");
         #endif

         static_assert(start != calibration_name.npos, "Bad NameOf adaptation");
         return start;
      }
      
      /// Analyze compiler stringification and find the right offset in order 
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the right          
      consteval size_t CalculateTypeRightOffset() {
         constexpr auto calibration_name = 
            WrappedTypeName<Oddly_Specific_TypeASFNWEAFNOLAWFNWAFK>();
         constexpr size_t start = CalculateTypeLeftOffset();

         #if LANGULUS_COMPILER(MSVC)
            return calibration_name.size() - start - 67;
         #elif LANGULUS_COMPILER(GCC)
            return calibration_name.size() - start - 38;
         #else
            return calibration_name.size() - start - 61;
         #endif
      }

      /// Analyze compiler stringification and find the left offset in order  
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the left           
      consteval size_t CalculateEnumLeftOffset() {
         constexpr auto calibration_name = 
            WrappedEnumName<Oddly_Specific_EnumASDOLSAJDPAFHOAF>();

         constexpr size_t start = calibration_name.find(
            "Langulus::RTTI::Inner::Oddly_Specific_EnumASDOLSAJDPAFHOAF");
         static_assert(start != calibration_name.npos, "Bad NameOf adaptation");
         return start;
      }

      /// Analyze compiler stringification and find the right offset in order 
      /// the shed the unnecessary emballage                                  
      ///   @return the number of characters to discard on the right          
      consteval size_t CalculateEnumRightOffset() {
         constexpr auto calibration_name = 
            WrappedEnumName<Oddly_Specific_EnumASDOLSAJDPAFHOAF>();
         constexpr size_t start = CalculateEnumLeftOffset();
         return calibration_name.size() - start - 58;
      }

      constexpr size_t CalibratedTypeLeftOffset  = CalculateTypeLeftOffset();
      constexpr size_t CalibratedTypeRightOffset = CalculateTypeRightOffset();
      constexpr size_t CalibratedEnumLeftOffset  = CalculateEnumLeftOffset();
      constexpr size_t CalibratedEnumRightOffset = CalculateEnumRightOffset();

      template<Literal>
      consteval auto Normalize();

      /// Skip all decorations in front and the back of a WrappedTypeName     
      ///   @tparam T - the typename to isolate                               
      ///   @tparam NORMALIZE - whether or not to normalize the typename to   
      ///      Langulus specification                                         
      ///   @return a compile-time string                                     
      template<class T, bool NORMALIZE = true>
      consteval auto IsolateTypename() {
         if constexpr (NORMALIZE and CTTI::Named<T>::Enabled)
            return CTTI::Named<T>::Name;
         // Move `const` next to pointers/references at the end of type 
         // Discards `volatile` - it shouldn't matter outside compiler  
         // Helps with better sorting reflected types                   
         else if constexpr (::std::is_const_v<T> or ::std::is_volatile_v<T>) {
            auto deptr = IsolateTypename<Decvq<T>, NORMALIZE>();
            if constexpr (not ::std::is_const_v<T>)
               return deptr;
            else
               return deptr + " const";
         }
         else if constexpr (::std::is_reference_v<T>) {
            auto deptr = IsolateTypename<Decvq<Deref<T>>, NORMALIZE>();
            if constexpr (not ::std::is_const_v<Deref<T>>)
               return deptr + "&";
            else
               return deptr + " const&";
         }
         else if constexpr (::std::is_pointer_v<T>) {
            auto deptr = IsolateTypename<Decvq<Deptr<T>>, NORMALIZE>();
            if constexpr (not ::std::is_const_v<Deptr<T>>)
               return deptr + "*";
            else
               return deptr + " const*";
         }
         else if constexpr (NORMALIZE and requires { T::CTTI_Named::Constant; })
            return T::CTTI_Named::Constant;
         else {
            constexpr auto name = WrappedTypeName<T>();
            constexpr size_t size = name.size();
            constexpr size_t left = CalibratedTypeLeftOffset;
            constexpr size_t right = CalibratedTypeRightOffset;
            static_assert(size > left + right, "Invalid type name");

            constexpr auto isolated = name.template substr<left, size - right - left>();
            if constexpr (not NORMALIZE) {
               if constexpr (::std::is_function_v<T>)
                  return "<" + isolated + ">";
               else
                  return isolated;
            }
            else {
               constexpr auto normalized = Normalize<isolated>();
               if constexpr (::std::is_function_v<T>)
                  return "<" + normalized + ">";
               else
                  return normalized;
            }
         }
      }

      /// Skip all decorations in front and back of a WrappedEnumName         
      ///   @tparam T - the constant to isolate                               
      ///   @tparam NORMALIZE - whether or not to normalize the constant to   
      ///      Langulus specification                                         
      ///   @return a compile-time string                                     
      template<auto T, bool NORMALIZE = true>
      consteval auto IsolateConstant() {
         if constexpr (NORMALIZE and CT::NamedValue<T>)
            return CTTI::NamedValue<T>::Name;
         else {
            constexpr auto name = WrappedEnumName<T>();
            constexpr auto size = name.size();
            constexpr auto left = CalibratedEnumLeftOffset;
            constexpr auto right = CalibratedEnumRightOffset;
            static_assert(size > left + right, "Invalid enum name");

            constexpr auto isolated = name.template substr<left, size - right - left>();
            if constexpr (NORMALIZE)
               return Normalize<isolated>();
            else
               return isolated;
         }
      }

      /// Check if a token transition happens at the beginning and the end of 
      /// a region inside a source. A token transition means, that the token  
      /// is surrounded by non-alphabetical symbols                           
      ///   @param source - data source                                       
      ///   @param lhs - start of the region                                  
      ///   @param rhs - end of the region                                    
      ///   @return true if a transition occurs at both points                
      constexpr bool IsTransition(auto source, size_t lhs, size_t rhs) {
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
      consteval size_t CountOccurences() {
         if constexpr (RHS.size() > LHS.size() or RHS.size() == 0)
            return 0;
         else {
            size_t occurences = 0;
            size_t cookie = 0;
            while (cookie + RHS.size() <= LHS.size()) {
               size_t scan = 0;
               while (scan < RHS.size()) {
                  if (LHS[cookie + scan] == RHS[scan]) {
                     ++scan;
                     continue;
                  }

                  break;
               }

               if (scan == RHS.size() and Inner::IsTransition(LHS, cookie, cookie + RHS.size())) {
                  cookie += RHS.size();
                  ++occurences;
               }
               else ++cookie;
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
            size_t fill = 0;
            size_t prev = 0;
            size_t curr = 0;
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
      #if LANGULUS_COMPILER(MSVC)
         constexpr auto a00 = Replace<SRC, Literal {"`anonymous-namespace'::"}, Literal {""}>();
      #elif LANGULUS_COMPILER(CLANG)
         constexpr auto a00 = Replace<SRC, Literal {"(anonymous namespace)::"}, Literal {""}>();
      #else
         constexpr auto b00 = Replace<SRC, Literal {"<unnamed>::"},   Literal {"" }>();
         constexpr auto a00 = Replace<b00, Literal {"{anonymous}::"}, Literal {"" }>();
      #endif
         constexpr auto a01 = Replace<a00, Literal {" *"},            Literal {"*"}>();
         constexpr auto a02 = Replace<a01, Literal {" &"},            Literal {"&"}>();
         constexpr auto a03 = Replace<a02, Literal {" >"},            Literal {">"}>();
         constexpr auto a04 = Replace<a03, Literal {" ("},            Literal {"("}>();
         constexpr auto a05 = Replace<a04, Literal {" )"},            Literal {")"}>();
         constexpr auto a06 = Replace<a05, Literal {" ["},            Literal {"["}>();
         constexpr auto a07 = Replace<a06, Literal {" ]"},            Literal {"]"}>();
         constexpr auto a08 = Replace<a07, Literal {"class "},        Literal {"" }>();
         constexpr auto a09 = Replace<a08, Literal {"struct "},       Literal {"" }>();
         constexpr auto a10 = Replace<a09, Literal {"enum "},         Literal {"" }>();
         constexpr auto a11 = Replace<a10, Literal {"(__cdecl *)"},   Literal {"" }>();

         // These types are stringified differently on some compilers   
         // `unsigned short` is longer than just `short`, and needs to  
         // be handled first                                            
         constexpr auto a12 = Replace<a11, IsolateTypename<uint8_t,  false>(), Literal {"uint8" }>();
         constexpr auto a13 = Replace<a12, IsolateTypename<uint16_t, false>(), Literal {"uint16"}>();
         constexpr auto a14 = Replace<a13, IsolateTypename<uint32_t, false>(), Literal {"uint32"}>();
         constexpr auto a15 = Replace<a14, IsolateTypename<uint64_t, false>(), Literal {"uint64"}>();

         constexpr auto a16 = Replace<a15, IsolateTypename<int8_t,   false>(), Literal {"int8"  }>();
         constexpr auto a17 = Replace<a16, IsolateTypename<int16_t,  false>(), Literal {"int16" }>();
         constexpr auto a18 = Replace<a17, IsolateTypename<int32_t,  false>(), Literal {"int32" }>();
         constexpr auto a19 = Replace<a18, IsolateTypename<int64_t,  false>(), Literal {"int64" }>();

         static_assert(IsASCII(a19), "Normalized typename isn't ASCII");
         return a19;
      }
      
      /// Get the last, most relevant part of a token that may or may not     
      /// have namespaces in it. Essentially finds last "::" that isn't       
      /// enclosed in a <template>, and skip forward to that                  
      ///   @param token - the token to scan                                  
      ///   @return the last token                                            
      constexpr size_t FindLastToken(const Token& token) noexcept {
         size_t depth = 0;
         for (size_t i = token.size() - 1; i < token.size(); --i) {
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
      return RTTI::Inner::IsolateTypename<T, false>();
   }
   
   /// Get the name of an enum value at compile-time                          
   ///   @tparam E - the constant to get the name of                          
   ///   @return a compile-time string                                        
   template<auto E>
   consteval auto CppNameOf() {
      return RTTI::Inner::IsolateConstant<E, false>();
   }
   

   /// Same as CppNameOf, but removes all namespaces at compile-time          
   ///   @tparam T - the type to get the name of                              
   ///   @return a compile-time string                                        
   template<class T>
   consteval auto LastCppNameOf() {
      // Find the last ':' symbol, that is not inside <...> scope       
      auto fullName = RTTI::Inner::IsolateTypename<T, false>();
      auto lastName = RTTI::Inner::FindLastToken(fullName);
      return fullName.substr(lastName);
   }

   /// Same as CppNameOf, but removes all namespaces at compile-time          
   ///   @tparam E - the enum to get the name of                              
   ///   @return a compile-time string                                        
   template<auto E>
   consteval auto LastCppNameOf() {
      // Find the last ':' symbol, that is not inside <...> scope       
      auto fullName = RTTI::Inner::IsolateConstant<E, false>();
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
      return RTTI::Inner::IsolateTypename<T>();
   }
   
   /// Get the name of an enum value at compile-time                          
   ///   @attention similarly named values in anonymous namespaces will result
   ///      in the same name. If this is not desired, specialize              
   ///      CTTI::NamedValue for each translation unit they appear in         
   ///   @tparam E - the value to get the name of                             
   ///   @return a compile-time string                                        
   template<auto E>
   consteval auto NameOf() {
      return RTTI::Inner::IsolateConstant<E>();
   }

} // namespace Langulus
