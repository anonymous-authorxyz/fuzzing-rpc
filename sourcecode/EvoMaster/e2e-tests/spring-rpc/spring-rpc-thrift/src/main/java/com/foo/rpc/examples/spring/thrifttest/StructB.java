/**
 * Autogenerated by Thrift Compiler (0.15.0)
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 */
package com.foo.rpc.examples.spring.thrifttest;

@SuppressWarnings({"cast", "rawtypes", "serial", "unchecked", "unused"})
@javax.annotation.Generated(value = "Autogenerated by Thrift Compiler (0.15.0)", date = "2021-12-08")
public class StructB implements org.apache.thrift.TBase<StructB, StructB._Fields>, java.io.Serializable, Cloneable, Comparable<StructB> {
  private static final org.apache.thrift.protocol.TStruct STRUCT_DESC = new org.apache.thrift.protocol.TStruct("StructB");

  private static final org.apache.thrift.protocol.TField AA_FIELD_DESC = new org.apache.thrift.protocol.TField("aa", org.apache.thrift.protocol.TType.STRUCT, (short)1);
  private static final org.apache.thrift.protocol.TField AB_FIELD_DESC = new org.apache.thrift.protocol.TField("ab", org.apache.thrift.protocol.TType.STRUCT, (short)2);

  private static final org.apache.thrift.scheme.SchemeFactory STANDARD_SCHEME_FACTORY = new StructBStandardSchemeFactory();
  private static final org.apache.thrift.scheme.SchemeFactory TUPLE_SCHEME_FACTORY = new StructBTupleSchemeFactory();

  public @org.apache.thrift.annotation.Nullable StructA aa; // optional
  public @org.apache.thrift.annotation.Nullable StructA ab; // required

  /** The set of fields this struct contains, along with convenience methods for finding and manipulating them. */
  public enum _Fields implements org.apache.thrift.TFieldIdEnum {
    AA((short)1, "aa"),
    AB((short)2, "ab");

    private static final java.util.Map<java.lang.String, _Fields> byName = new java.util.HashMap<java.lang.String, _Fields>();

    static {
      for (_Fields field : java.util.EnumSet.allOf(_Fields.class)) {
        byName.put(field.getFieldName(), field);
      }
    }

    /**
     * Find the _Fields constant that matches fieldId, or null if its not found.
     */
    @org.apache.thrift.annotation.Nullable
    public static _Fields findByThriftId(int fieldId) {
      switch(fieldId) {
        case 1: // AA
          return AA;
        case 2: // AB
          return AB;
        default:
          return null;
      }
    }

    /**
     * Find the _Fields constant that matches fieldId, throwing an exception
     * if it is not found.
     */
    public static _Fields findByThriftIdOrThrow(int fieldId) {
      _Fields fields = findByThriftId(fieldId);
      if (fields == null) throw new java.lang.IllegalArgumentException("Field " + fieldId + " doesn't exist!");
      return fields;
    }

    /**
     * Find the _Fields constant that matches name, or null if its not found.
     */
    @org.apache.thrift.annotation.Nullable
    public static _Fields findByName(java.lang.String name) {
      return byName.get(name);
    }

    private final short _thriftId;
    private final java.lang.String _fieldName;

    _Fields(short thriftId, java.lang.String fieldName) {
      _thriftId = thriftId;
      _fieldName = fieldName;
    }

    public short getThriftFieldId() {
      return _thriftId;
    }

    public java.lang.String getFieldName() {
      return _fieldName;
    }
  }

  // isset id assignments
  private static final _Fields optionals[] = {_Fields.AA};
  public static final java.util.Map<_Fields, org.apache.thrift.meta_data.FieldMetaData> metaDataMap;
  static {
    java.util.Map<_Fields, org.apache.thrift.meta_data.FieldMetaData> tmpMap = new java.util.EnumMap<_Fields, org.apache.thrift.meta_data.FieldMetaData>(_Fields.class);
    tmpMap.put(_Fields.AA, new org.apache.thrift.meta_data.FieldMetaData("aa", org.apache.thrift.TFieldRequirementType.OPTIONAL, 
        new org.apache.thrift.meta_data.StructMetaData(org.apache.thrift.protocol.TType.STRUCT, StructA.class)));
    tmpMap.put(_Fields.AB, new org.apache.thrift.meta_data.FieldMetaData("ab", org.apache.thrift.TFieldRequirementType.REQUIRED, 
        new org.apache.thrift.meta_data.StructMetaData(org.apache.thrift.protocol.TType.STRUCT, StructA.class)));
    metaDataMap = java.util.Collections.unmodifiableMap(tmpMap);
    org.apache.thrift.meta_data.FieldMetaData.addStructMetaDataMap(StructB.class, metaDataMap);
  }

  public StructB() {
  }

  public StructB(
    StructA ab)
  {
    this();
    this.ab = ab;
  }

  /**
   * Performs a deep copy on <i>other</i>.
   */
  public StructB(StructB other) {
    if (other.isSetAa()) {
      this.aa = new StructA(other.aa);
    }
    if (other.isSetAb()) {
      this.ab = new StructA(other.ab);
    }
  }

  public StructB deepCopy() {
    return new StructB(this);
  }

  @Override
  public void clear() {
    this.aa = null;
    this.ab = null;
  }

  @org.apache.thrift.annotation.Nullable
  public StructA getAa() {
    return this.aa;
  }

  public StructB setAa(@org.apache.thrift.annotation.Nullable StructA aa) {
    this.aa = aa;
    return this;
  }

  public void unsetAa() {
    this.aa = null;
  }

  /** Returns true if field aa is set (has been assigned a value) and false otherwise */
  public boolean isSetAa() {
    return this.aa != null;
  }

  public void setAaIsSet(boolean value) {
    if (!value) {
      this.aa = null;
    }
  }

  @org.apache.thrift.annotation.Nullable
  public StructA getAb() {
    return this.ab;
  }

  public StructB setAb(@org.apache.thrift.annotation.Nullable StructA ab) {
    this.ab = ab;
    return this;
  }

  public void unsetAb() {
    this.ab = null;
  }

  /** Returns true if field ab is set (has been assigned a value) and false otherwise */
  public boolean isSetAb() {
    return this.ab != null;
  }

  public void setAbIsSet(boolean value) {
    if (!value) {
      this.ab = null;
    }
  }

  public void setFieldValue(_Fields field, @org.apache.thrift.annotation.Nullable java.lang.Object value) {
    switch (field) {
    case AA:
      if (value == null) {
        unsetAa();
      } else {
        setAa((StructA)value);
      }
      break;

    case AB:
      if (value == null) {
        unsetAb();
      } else {
        setAb((StructA)value);
      }
      break;

    }
  }

  @org.apache.thrift.annotation.Nullable
  public java.lang.Object getFieldValue(_Fields field) {
    switch (field) {
    case AA:
      return getAa();

    case AB:
      return getAb();

    }
    throw new java.lang.IllegalStateException();
  }

  /** Returns true if field corresponding to fieldID is set (has been assigned a value) and false otherwise */
  public boolean isSet(_Fields field) {
    if (field == null) {
      throw new java.lang.IllegalArgumentException();
    }

    switch (field) {
    case AA:
      return isSetAa();
    case AB:
      return isSetAb();
    }
    throw new java.lang.IllegalStateException();
  }

  @Override
  public boolean equals(java.lang.Object that) {
    if (that instanceof StructB)
      return this.equals((StructB)that);
    return false;
  }

  public boolean equals(StructB that) {
    if (that == null)
      return false;
    if (this == that)
      return true;

    boolean this_present_aa = true && this.isSetAa();
    boolean that_present_aa = true && that.isSetAa();
    if (this_present_aa || that_present_aa) {
      if (!(this_present_aa && that_present_aa))
        return false;
      if (!this.aa.equals(that.aa))
        return false;
    }

    boolean this_present_ab = true && this.isSetAb();
    boolean that_present_ab = true && that.isSetAb();
    if (this_present_ab || that_present_ab) {
      if (!(this_present_ab && that_present_ab))
        return false;
      if (!this.ab.equals(that.ab))
        return false;
    }

    return true;
  }

  @Override
  public int hashCode() {
    int hashCode = 1;

    hashCode = hashCode * 8191 + ((isSetAa()) ? 131071 : 524287);
    if (isSetAa())
      hashCode = hashCode * 8191 + aa.hashCode();

    hashCode = hashCode * 8191 + ((isSetAb()) ? 131071 : 524287);
    if (isSetAb())
      hashCode = hashCode * 8191 + ab.hashCode();

    return hashCode;
  }

  @Override
  public int compareTo(StructB other) {
    if (!getClass().equals(other.getClass())) {
      return getClass().getName().compareTo(other.getClass().getName());
    }

    int lastComparison = 0;

    lastComparison = java.lang.Boolean.compare(isSetAa(), other.isSetAa());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetAa()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.aa, other.aa);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    lastComparison = java.lang.Boolean.compare(isSetAb(), other.isSetAb());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetAb()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.ab, other.ab);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    return 0;
  }

  @org.apache.thrift.annotation.Nullable
  public _Fields fieldForId(int fieldId) {
    return _Fields.findByThriftId(fieldId);
  }

  public void read(org.apache.thrift.protocol.TProtocol iprot) throws org.apache.thrift.TException {
    scheme(iprot).read(iprot, this);
  }

  public void write(org.apache.thrift.protocol.TProtocol oprot) throws org.apache.thrift.TException {
    scheme(oprot).write(oprot, this);
  }

  @Override
  public java.lang.String toString() {
    java.lang.StringBuilder sb = new java.lang.StringBuilder("StructB(");
    boolean first = true;

    if (isSetAa()) {
      sb.append("aa:");
      if (this.aa == null) {
        sb.append("null");
      } else {
        sb.append(this.aa);
      }
      first = false;
    }
    if (!first) sb.append(", ");
    sb.append("ab:");
    if (this.ab == null) {
      sb.append("null");
    } else {
      sb.append(this.ab);
    }
    first = false;
    sb.append(")");
    return sb.toString();
  }

  public void validate() throws org.apache.thrift.TException {
    // check for required fields
    if (ab == null) {
      throw new org.apache.thrift.protocol.TProtocolException("Required field 'ab' was not present! Struct: " + toString());
    }
    // check for sub-struct validity
    if (aa != null) {
      aa.validate();
    }
    if (ab != null) {
      ab.validate();
    }
  }

  private void writeObject(java.io.ObjectOutputStream out) throws java.io.IOException {
    try {
      write(new org.apache.thrift.protocol.TCompactProtocol(new org.apache.thrift.transport.TIOStreamTransport(out)));
    } catch (org.apache.thrift.TException te) {
      throw new java.io.IOException(te);
    }
  }

  private void readObject(java.io.ObjectInputStream in) throws java.io.IOException, java.lang.ClassNotFoundException {
    try {
      read(new org.apache.thrift.protocol.TCompactProtocol(new org.apache.thrift.transport.TIOStreamTransport(in)));
    } catch (org.apache.thrift.TException te) {
      throw new java.io.IOException(te);
    }
  }

  private static class StructBStandardSchemeFactory implements org.apache.thrift.scheme.SchemeFactory {
    public StructBStandardScheme getScheme() {
      return new StructBStandardScheme();
    }
  }

  private static class StructBStandardScheme extends org.apache.thrift.scheme.StandardScheme<StructB> {

    public void read(org.apache.thrift.protocol.TProtocol iprot, StructB struct) throws org.apache.thrift.TException {
      org.apache.thrift.protocol.TField schemeField;
      iprot.readStructBegin();
      while (true)
      {
        schemeField = iprot.readFieldBegin();
        if (schemeField.type == org.apache.thrift.protocol.TType.STOP) { 
          break;
        }
        switch (schemeField.id) {
          case 1: // AA
            if (schemeField.type == org.apache.thrift.protocol.TType.STRUCT) {
              struct.aa = new StructA();
              struct.aa.read(iprot);
              struct.setAaIsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          case 2: // AB
            if (schemeField.type == org.apache.thrift.protocol.TType.STRUCT) {
              struct.ab = new StructA();
              struct.ab.read(iprot);
              struct.setAbIsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          default:
            org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
        }
        iprot.readFieldEnd();
      }
      iprot.readStructEnd();

      // check for required fields of primitive type, which can't be checked in the validate method
      struct.validate();
    }

    public void write(org.apache.thrift.protocol.TProtocol oprot, StructB struct) throws org.apache.thrift.TException {
      struct.validate();

      oprot.writeStructBegin(STRUCT_DESC);
      if (struct.aa != null) {
        if (struct.isSetAa()) {
          oprot.writeFieldBegin(AA_FIELD_DESC);
          struct.aa.write(oprot);
          oprot.writeFieldEnd();
        }
      }
      if (struct.ab != null) {
        oprot.writeFieldBegin(AB_FIELD_DESC);
        struct.ab.write(oprot);
        oprot.writeFieldEnd();
      }
      oprot.writeFieldStop();
      oprot.writeStructEnd();
    }

  }

  private static class StructBTupleSchemeFactory implements org.apache.thrift.scheme.SchemeFactory {
    public StructBTupleScheme getScheme() {
      return new StructBTupleScheme();
    }
  }

  private static class StructBTupleScheme extends org.apache.thrift.scheme.TupleScheme<StructB> {

    @Override
    public void write(org.apache.thrift.protocol.TProtocol prot, StructB struct) throws org.apache.thrift.TException {
      org.apache.thrift.protocol.TTupleProtocol oprot = (org.apache.thrift.protocol.TTupleProtocol) prot;
      struct.ab.write(oprot);
      java.util.BitSet optionals = new java.util.BitSet();
      if (struct.isSetAa()) {
        optionals.set(0);
      }
      oprot.writeBitSet(optionals, 1);
      if (struct.isSetAa()) {
        struct.aa.write(oprot);
      }
    }

    @Override
    public void read(org.apache.thrift.protocol.TProtocol prot, StructB struct) throws org.apache.thrift.TException {
      org.apache.thrift.protocol.TTupleProtocol iprot = (org.apache.thrift.protocol.TTupleProtocol) prot;
      struct.ab = new StructA();
      struct.ab.read(iprot);
      struct.setAbIsSet(true);
      java.util.BitSet incoming = iprot.readBitSet(1);
      if (incoming.get(0)) {
        struct.aa = new StructA();
        struct.aa.read(iprot);
        struct.setAaIsSet(true);
      }
    }
  }

  private static <S extends org.apache.thrift.scheme.IScheme> S scheme(org.apache.thrift.protocol.TProtocol proto) {
    return (org.apache.thrift.scheme.StandardScheme.class.equals(proto.getScheme()) ? STANDARD_SCHEME_FACTORY : TUPLE_SCHEME_FACTORY).getScheme();
  }
}

