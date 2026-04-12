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
#include "Utils/ASCII.hpp"
#include <string>
#include <stdexcept>


namespace Langulus::CTTI
{
   ///                                                                        
   /// The following are some manual overrides that make stuff consistent     
   /// across different compilers                                             
   ///                                                                        
   template<>
   struct Named<int8_t> {
      static constexpr Literal Name = "int8";
   };

   template<>
   struct Named<int16_t> {
      static constexpr Literal Name = "int16";
   };

   template<>
   struct Named<int32_t> {
      static constexpr Literal Name = "int32";
   };

   template<>
   struct Named<int64_t> {
      static constexpr Literal Name = "int64";
   };

   template<>
   struct Named<uint8_t> {
      static constexpr Literal Name = "uint8";
   };

   template<>
   struct Named<uint16_t> {
      static constexpr Literal Name = "uint16";
   };

   template<>
   struct Named<uint32_t> {
      static constexpr Literal Name = "uint32";
   };

   template<>
   struct Named<uint64_t> {
      static constexpr Literal Name = "uint64";
   };
}

namespace Langulus::RTTI
{
   using Lowercase = ::std::string;

   namespace Inner
   {
      /// Reserved keywords - you're not allowed to name types after them     
      constexpr Token ReservedKeywords[] = {
         "null", "notype", "notag", "noverb", "novalue", "const"
      };

      /// Check if a name is reserved                                         
      consteval bool IsReserved(const Token& name) {
         for (auto& reserved : ReservedKeywords) {
            if (name.size() != reserved.size())
               continue;

            size_t i = 0;
            for (; i < name.size(); ++i) {
               if (ToLowercase(name[i]) != reserved[i])
                  break;
            }

            if (i == name.size())
               return true;
         }
         return false;
      }

      /// Types used for pattern matching while isolating typenames.          
      /// These need to be in exactly this namespace to avoid corner cases.   
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
      // ReSharper disable once CppDFAUnreachableFunctionCall           
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
      // ReSharper disable once CppDFAUnreachableFunctionCall           
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
      constexpr auto Normalize();

      /// Skip all decorations in front and the back of a WrappedTypeName     
      ///   @tparam T the typename to isolate                                 
      ///   @tparam NORMALIZE whether or not to normalize the typename to     
      ///      Langulus specification                                         
      ///   @tparam NAMED whether or not to apply any CTTI named traits       
      ///   @return a compile-time string                                     
      template<class T, bool NORMALIZE = true, bool NAMED = true>
      consteval auto IsolateTypename() {
         if constexpr (NAMED and CT::Complete<CTTI::Named<T>>) {
            // Custom name by CTTI::Named specialization                
            static_assert(IsKeyword(CTTI::Named<T>::Name),
               "Not a valid CTTI::Named - "
               "must be ASCII, starting with an alphabetical symbol, "
               "and must not contain any spaces or operators"
            );
            static_assert(not IsReserved(CTTI::Named<T>::Name),
               "Not a valid CTTI::Named - token is reserved"
            );
            return CTTI::Named<T>::Name;
         }
         else if constexpr (::std::is_const_v<T> or ::std::is_volatile_v<T>) {
            // Move `const` next to pointers/references at the end of   
            // type. Discards `volatile` - it shouldn't matter outside  
            // compiler. Helps with better sorting of reflected types   
            auto deptr = IsolateTypename<Decvq<T>, NORMALIZE, NAMED>();
            if constexpr (not ::std::is_const_v<T>)
               return deptr;
            else
               return deptr + " const";
         }
         else if constexpr (::std::is_reference_v<T>) {
            // Append & or const& to the back                           
            auto deptr = IsolateTypename<Decvq<Deref<T>>, NORMALIZE, NAMED>();
            if constexpr (not ::std::is_const_v<Deref<T>>)
               return deptr + "&";
            else
               return deptr + " const&";
         }
         else if constexpr (::std::is_bounded_array_v<T>) {
            // Append extent                                            
            auto deext = IsolateTypename<Deext<T>, NORMALIZE, NAMED>();
            constexpr auto ext = ::std::extent_v<T>;
            static_assert(ext < 1000000, "Extent is too big");
            if constexpr (ext > 99999) {
               return deext + "[" + static_cast<char>('0' + ext/100000)
                                  + static_cast<char>('0' + ext/10000)
                                  + static_cast<char>('0' + ext/1000)
                                  + static_cast<char>('0' + ext/100)
                                  + static_cast<char>('0' + ext/10)
                                  + static_cast<char>('0' + ext) + "]";
            }
            else if constexpr (ext > 9999) {
               return deext + "[" + static_cast<char>('0' + ext/10000)
                                  + static_cast<char>('0' + ext/1000)
                                  + static_cast<char>('0' + ext/100)
                                  + static_cast<char>('0' + ext/10)
                                  + static_cast<char>('0' + ext) + "]";
            }
            else if constexpr (ext > 999) {
               return deext + "[" + static_cast<char>('0' + ext/1000)
                                  + static_cast<char>('0' + ext/100)
                                  + static_cast<char>('0' + ext/10)
                                  + static_cast<char>('0' + ext) + "]";               
            }
            else if constexpr (ext > 99) {
               return deext + "[" + static_cast<char>('0' + ext/100)
                                  + static_cast<char>('0' + ext/10)
                                  + static_cast<char>('0' + ext) + "]";               
            }
            else if constexpr (ext > 9) {
               return deext + "[" + static_cast<char>('0' + ext/10)
                                  + static_cast<char>('0' + ext) + "]";
            }
            else return deext + "[" + static_cast<char>('0' + ext) + "]";
         }
         else if constexpr (::std::is_pointer_v<T>) {
            // Append * or const* to the back                           
            auto deptr = IsolateTypename<Decvq<Deptr<T>>, NORMALIZE, NAMED>();
            if constexpr (not ::std::is_const_v<Deptr<T>>)
               return deptr + "*";
            else
               return deptr + " const*";
         }
         else if constexpr (NAMED and requires { T::CTTI_Named::Enabled; }) {
            if constexpr (T::CTTI_Named::Enabled) {
               // Custom name taken from T::CTTI_Named member           
               static_assert(IsKeyword(T::CTTI_Named::Constant),
                  "Not a valid CTTI_Named - "
                  "must be ASCII, starting with an alphabetical symbol, "
                  "and must not contain any spaces or operators"
               );
               static_assert(not IsReserved(T::CTTI_Named::Constant),
                  "Not a valid CTTI_Named - token is reserved"
               );
               return T::CTTI_Named::Constant;
            }
            else return IsolateTypename<T, NORMALIZE, false>();
         }
         else {
            // Extract the C++ name, normalize it if required           
            constexpr auto name = WrappedTypeName<T>();
            constexpr size_t size = name.size();
            constexpr size_t left = CalibratedTypeLeftOffset;
            constexpr size_t right = CalibratedTypeRightOffset;
            static_assert(size > left + right, "Invalid type name");

            constexpr auto isolated = name.substr(left, size - right - left);
            static_assert(not IsReserved(isolated),
               "Not a valid C++ name - token is reserved"
            );

            if constexpr (not NORMALIZE) {
               if constexpr (::std::is_function_v<T>)
                  return "<" + isolated + ">";
               else
                  return isolated;
            }
            else {
               const auto normalized = Normalize<isolated>();
               if constexpr (::std::is_function_v<T>)
                  return "<" + normalized + ">";
               else
                  return normalized;
            }
         }
      }

      /// Skip all decorations in front and back of a WrappedEnumName         
      ///   @tparam T the constant to isolate                                 
      ///   @tparam NORMALIZE whether or not to normalize the constant to     
      ///      Langulus specification                                         
      ///   @tparam NAMED whether or not to apply any CTTI named traits       
      ///   @return a compile-time string                                     
      template<auto T, bool NORMALIZE = true, bool NAMED = true>
      consteval auto IsolateConstant() {
         if constexpr (NAMED and CT::NamedValue<T>) {
            // Custom name by specializing CTTI::NamedValue             
            static_assert(IsKeyword(CTTI::NamedValue<T>::Name),
               "Not a valid CTTI::NamedValue - "
               "must be ASCII, starting with an alphabetical symbol, "
               "and must not contain any spaces or operators"
            );
            static_assert(not IsReserved(CTTI::NamedValue<T>::Name),
               "Not a valid CTTI::NamedValue - token is reserved"
            );
            return CTTI::NamedValue<T>::Name;
         }
         else {
            // Extract the C++ name and normalize it if required        
            constexpr auto name = WrappedEnumName<T>();
            constexpr auto size = name.size();
            constexpr auto left = CalibratedEnumLeftOffset;
            constexpr auto right = CalibratedEnumRightOffset;
            static_assert(size > left + right, "Invalid enum name");

            constexpr auto isolated = name.substr(left, size - right - left);
            static_assert(not IsReserved(isolated),
               "Not a valid C++ value name - token is reserved"
            );

            if constexpr (NORMALIZE)
               return Normalize<isolated>();
            else
               return isolated;
         }
      }

      constexpr Literal uint8_t_token  = IsolateTypename<uint8_t,  false, false>();
      constexpr Literal uint16_t_token = IsolateTypename<uint16_t, false, false>();
      constexpr Literal uint32_t_token = IsolateTypename<uint32_t, false, false>();
      constexpr Literal uint64_t_token = IsolateTypename<uint64_t, false, false>();
      constexpr Literal int8_t_token   = IsolateTypename<int8_t,   false, false>();
      constexpr Literal int16_t_token  = IsolateTypename<int16_t,  false, false>();
      constexpr Literal int32_t_token  = IsolateTypename<int32_t,  false, false>();
      constexpr Literal int64_t_token  = IsolateTypename<int64_t,  false, false>();
      
      /// Replace these patterns when normalizing names                       
      /// @attention when having similar tokens to replace, order             
      ///    them correctly, with longer ones replaced first                  
      /// @attention replacement will not commence, if IsTransition           
      ///    isn't satisifed                                                  
      constexpr struct ReplacePattern {
         Token what;
         Token with;
      } ReplacePatterns[] = {
         #if LANGULUS_COMPILER(MSVC)
            {"`anonymous-namespace'::", ""}, //TODO disallow reflecting anonymous namespaces thingies through here somewhere
         #elif LANGULUS_COMPILER(CLANG)
            {"(anonymous namespace)::", ""},
         #else
            {"<unnamed>::",   ""},
            {"{anonymous}::", ""},
         #endif

         {" *",           "*"},
         {" &",           "&"},
         {" >",           ">"},
         {" (",           "("},
         {" )",           ")"},
         {" [",           "["},
         {" ]",           "]"},
         {"class ",       "" },
         {"struct ",      "" },
         {"enum ",        "" },
         {"(__cdecl *)",  "" },
         
         // These types are stringified differently on some compilers   
         // `unsigned short` is longer than just `short`, and needs to  
         // be handled first                                            
         {uint8_t_token,  "uint8"  },
         {uint16_t_token, "uint16" },
         {uint32_t_token, "uint32" },
         {uint64_t_token, "uint64" },
         {int8_t_token,   "int8"   },
         {int16_t_token,  "int16"  },
         {int32_t_token,  "int32"  },
         {int64_t_token,  "int64"  }
      };
      
      /// Decide buffer size by checking all replacement patterns             
      ///   @tparam SRC search where?                                         
      constexpr size_t DecideBufferSize(const CT::Literal auto& SRC) {
         const size_t src_size = SRC.size();
         size_t result = 0;
         for (const auto& pattern : ReplacePatterns) {
            size_t occurences = 0;
            size_t cookie = 0;
            while (cookie + pattern.what.size() <= src_size) {
               size_t scan = 0;
               while (scan < pattern.what.size()) {
                  if (SRC[cookie + scan] == pattern.what[scan]) {
                     ++scan;
                     continue;
                  }
                  break;
               }

               if (scan == pattern.what.size()
               and IsTransition(SRC, cookie, cookie + pattern.what.size())) {
                  cookie += pattern.what.size();
                  ++occurences;
               }
               else ++cookie;
            }
            
            const auto candidate = src_size
               - occurences * pattern.what.size()
               + occurences * pattern.with.size();
            
            if (candidate > result)
               result = candidate;
         }
         return (result < SRC.ArraySize ? SRC.ArraySize : result);
      }
      
      /// Normalize a type/enum/function name at runtime                      
      ///   @tparam SRC the token to normalize                                
      ///   @return new literal                                               
      inline ::std::string NormalizeAtRuntime(const Token& SRC) {
         if (not IsASCII(SRC))
            throw ::std::runtime_error {"Token isn't ASCII"};
         ::std::string result {SRC};
   
         for (const auto& pattern : ReplacePatterns) {
            size_t fill = 0;
            size_t prev = 0;
            size_t curr = result.find(pattern.what, 0);
            size_t already_replaced = not pattern.with.empty()
               ? result.find(pattern.with, 0)
               : result.npos;
            if (curr == result.npos or not IsTransition(result, curr, curr + pattern.what.size())
            or (already_replaced != result.npos and curr == already_replaced))
               continue;
            
            ::std::string buffer;
            buffer.resize(result.size());
            do {
               while (curr > prev) {
                  // Copy anything we've skipped                        
                  buffer[fill++] = result[prev++];
               }

               // Replace                                               
               buffer.resize(curr + pattern.with.size());
               for (char c : pattern.with)
                  buffer[fill++] = c;
               prev += pattern.what.size();
               
               curr = result.find(pattern.what, prev);
               already_replaced = not pattern.with.empty()
                  ? result.find(pattern.with, prev)
                  : result.npos;
            }
            while (curr != result.npos
            and   (already_replaced == result.npos or curr != already_replaced));
            
            while (prev < result.size()) {
               // Copy any remaining trailing data                      
               buffer.resize(fill + (result.size() - prev));
               buffer[fill++] = result[prev++];
            }
            
            result = LglsMov(buffer);
         }
         return result;
      }
      
      /// Normalize a type/enum/function name                                 
      ///   @tparam SRC the literal to normalize                              
      ///   @return new literal                                               
      template<Literal SRC>
      constexpr auto Normalize() {
         static_assert(IsASCII(SRC), "Literal isn't ASCII");
         Literal<char, DecideBufferSize(SRC)> result {SRC};
         
         for (const auto& pattern : ReplacePatterns) {
            size_t fill = 0;
            size_t prev = 0;
            size_t curr = result.find(pattern.what, 0);
            size_t already_replaced = not pattern.with.empty()
               ? result.find(pattern.with, 0)
               : result.npos;
            if (curr == result.npos or not IsTransition(result, curr, curr + pattern.what.size())
            or (already_replaced != result.npos and curr == already_replaced))
               continue;
            
            decltype(result) buffer;
            do {
               while (curr > prev) {
                  // Copy anything we've skipped                        
                  buffer[fill++] = result[prev++];
               }

               // Replace                                               
               for (char c : pattern.with)
                  buffer[fill++] = c;
               prev += pattern.what.size();

               curr = result.find(pattern.what, prev);
               already_replaced = not pattern.with.empty()
                  ? result.find(pattern.with, prev)
                  : result.npos;
            }
            while (curr != result.npos
            and   (already_replaced == result.npos or curr != already_replaced));
            
            while (prev < result.size()) {
               // Copy any remaining trailing data                      
               buffer[fill++] = result[prev++];
            }
            
            buffer[fill] = 0;
            result = buffer;
         }
         return result;
      }
      
      /// Get the last, most relevant part of a token that may or may not     
      /// have namespaces in it. Essentially finds last "::" that isn't       
      /// enclosed in a <template>, and skip forward to that.                 
      ///   @param token the token to scan                                    
      ///   @return the last token                                            
      // ReSharper disable once CppDFAUnreachableFunctionCall           
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
   }
}

namespace Langulus
{
   /// Get the name of a type, templated or not, with consistently named      
   /// template arguments, even if nested, at compile-time                    
   ///   @tparam T the type to get the name of                                
   ///   @tparam NORMALIZE whether to normalize name so that it is the same   
   ///      across compilers. This costs a lot of build time and is rarely    
   ///      used for C++ names                                                
   ///   @return a compile-time string                                        
   template<class T, bool NORMALIZE = false>
   constexpr auto CppNameOf() {
      return RTTI::Inner::IsolateTypename<T, NORMALIZE, false>();
   }
   
   /// Get the name of an enum value at compile-time                          
   ///   @tparam E the constant to get the name of                            
   ///   @tparam NORMALIZE whether to normalize name so that it is the same   
   ///      across compilers. This costs a lot of build time and is rarely    
   ///      used for C++ names                                                
   ///   @return a compile-time string                                        
   template<auto E, bool NORMALIZE = false>
   constexpr auto CppNameOf() {
      return RTTI::Inner::IsolateConstant<E, NORMALIZE, false>();
   }

   /// Same as CppNameOf, but removes all namespaces at compile-time          
   ///   @tparam T the type to get the name of                                
   ///   @tparam NORMALIZE whether to normalize name so that it is the same   
   ///      across compilers. This costs a lot of build time and is rarely    
   ///      used for C++ names                                                
   ///   @return a compile-time string                                        
   template<class T, bool NORMALIZE = false>
   constexpr auto LastCppNameOf() {
      // Find the last ':' symbol, that is not inside <...> scope       
      auto fullName = RTTI::Inner::IsolateTypename<T, NORMALIZE, false>();
      auto lastName = RTTI::Inner::FindLastToken(fullName);
      return fullName.substr(lastName);
   }

   /// Same as CppNameOf, but removes all namespaces at compile-time          
   ///   @tparam E the enum to get the name of                                
   ///   @tparam NORMALIZE whether to normalize name so that it is the same   
   ///      across compilers. This costs a lot of build time and is rarely    
   ///      used for C++ names                                                
   ///   @return a compile-time string                                        
   template<auto E, bool NORMALIZE = false>
   constexpr auto LastCppNameOf() {
      // Find the last ':' symbol, that is not inside <...> scope       
      auto fullName = RTTI::Inner::IsolateConstant<E, NORMALIZE, false>();
      auto lastName = RTTI::Inner::FindLastToken(fullName);
      return fullName.substr(lastName);
   }

   /// Get the name of a type at compile-time.                                
   /// Considers CTTI::Named, or fallbacks to the C++ name.                   
   /// If you want to avoid custom names, use CppNameOf directly instead.     
   ///   @attention similarly named types in anonymous namespaces will result 
   ///      in the same name. If this is not desired disable NORMALIZE, or    
   ///      specialize CTTI::Named for each translation unit the type         
   ///      appears in                                                        
   ///   @tparam T the type to get the name of                                
   ///   @tparam NORMALIZE whether to normalize name so that it is the same   
   ///      across compilers. This costs a lot of build time, so you might    
   ///      want to do it at runtime instead if build time becomes an issue   
   ///      See Langulus::RTTI::Inner::NormalizeAtRuntime                     
   ///   @return a compile-time string                                        
   ///   @return a compile-time string                                        
   template<class T, bool NORMALIZE = true>
   constexpr auto NameOf() {
      return RTTI::Inner::IsolateTypename<T, NORMALIZE>();
   }
   
   /// Get the name of an enum value at compile-time.                         
   /// Considers CTTI::NamedValue, or fallbacks to the C++ name.              
   ///   @attention similarly named values in anonymous namespaces will result
   ///      in the same name. If this is not desired disable NORMALIZE, or    
   ///      specialize CTTI::NamedValue for each translation unit the value   
   ///      appears in                                                        
   ///   @tparam E the value to get the name of                               
   ///   @tparam NORMALIZE whether to normalize name so that it is the same   
   ///      across compilers. This costs a lot of build time, so you might    
   ///      want to do it at runtime instead if build time becomes an issue   
   ///      See Langulus::RTTI::Inner::NormalizeAtRuntime                     
   ///   @return a compile-time string                                        
   template<auto E, bool NORMALIZE = true>
   constexpr auto NameOf() {
      return RTTI::Inner::IsolateConstant<E, NORMALIZE>();
   }
}
